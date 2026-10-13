module Main (main) where

import CatDB
import Demo
import Control.Exception (bracket)
import Data.Aeson hiding (Error, Object)
import qualified Data.ByteString.Lazy as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (hSetEncoding, stdout, stderr, utf8)

data Document = Document SchemaDef InstanceDef
instance ToJSON Document where
  toJSON (Document s i) = object ["schema" .= s,"instance" .= i]
instance FromJSON Document where
  parseJSON = withObject "database document" $ \o -> Document <$> o .: "schema" <*> o .: "instance"
data Spec = Spec SchemaDef SchemaDef MappingDef
instance FromJSON Spec where
  parseJSON = withObject "migration specification" $ \o -> Spec <$> o .: "source" <*> o .: "target" <*> o .: "mapping"

orDie :: Show e => Either e a -> IO a
orDie = either (die . show) pure

readJson :: FromJSON a => FilePath -> IO a
readJson path = L.readFile path >>= orDie . eitherDecode

emit :: ToJSON a => a -> IO ()
emit value = L.putStr (encode value <> "\n")

document :: Instance -> Document
document i = Document (definition (instanceSchema i)) (instanceData i)

using :: FilePath -> (Database -> IO a) -> IO a
using path = bracket (openDatabase path >>= orDie) (\db -> closeDatabase db >>= orDie)

create :: FilePath -> Instance -> IO ()
create path i = bracket (createDatabase path i >>= orDie) (\db -> closeDatabase db >>= orDie) (const (pure ()))

readMapping :: FilePath -> IO Mapping
readMapping path = do
  Spec s t f <- readJson path
  c <- orDie (schema s)
  d <- orDie (schema t)
  orDie (mapping c d f)

migration :: String -> Mapping -> Instance -> Either Error Instance
migration "delta" = delta
migration "sigma" = \f i -> sigmaInstance <$> sigma f i
migration "pi" = \f i -> piInstance <$> piMigration f i
migration _ = \_ _ -> Left (InternalInvariant "expected delta, sigma or pi")

main :: IO ()
main = do
  hSetEncoding stdout utf8
  hSetEncoding stderr utf8
  args <- getArgs
  case args of
    ["init",file,json] -> do
      Document s d <- readJson json
      i <- orDie (schema s >>= \c -> instanceOf c d)
      create file i
    ["dump",file] -> using file $ \db -> snapshot db >>= orDie >>= emit . document
    ["apply",file,json] -> do
      ops <- readJson json
      using file $ \db -> commit db ops >>= orDie
    "path":file:o:key:as -> using file $ \db -> do
      i <- snapshot db >>= orDie
      orDie (evaluate i (Path (Object (T.pack o)) (map (Arrow . T.pack) as)) (Element (T.pack key))) >>= emit
    "rows":file:o:paths -> using file $ \db -> do
      i <- snapshot db >>= orDie
      let obj = Object (T.pack o)
          columns = [(T.pack p,Path obj (map Arrow (T.splitOn "." (T.pack p)))) | p <- paths]
      orDie (rows i obj columns) >>= emit
    ["query",file,kind,spec] -> using file $ \db -> do
      f <- readMapping spec
      i <- snapshot db >>= orDie
      orDie (migration kind f i) >>= emit . document
    ["query",file,kind,spec,out] -> using file $ \db -> do
      f <- readMapping spec
      i <- snapshot db >>= orDie
      orDie (migration kind f i) >>= create out
    ["migrate",file,kind,spec] -> using file $ \db -> do
      f <- readMapping spec
      transaction db (\i -> (,) () <$> migration kind f i) >>= orDie
    ["copy",file,out] -> using file $ \db -> copyDatabase db out >>= orDie
    ["check",file] -> using file $ \db -> do
      i <- snapshot db >>= orDie
      n <- recoveredBytes db >>= orDie
      emit (object ["valid" .= True,"recoveredBytes" .= n,"objects" .= M.size (sets (instanceData i))])
    ["todo",file,"init"] -> orDie todoInstance >>= create file
    ["todo",file,"add",key,project,owner,title] -> using file $ \db ->
      transaction db (\i -> (,) () <$> addTask (Element (T.pack key)) (Element (T.pack project))
                        (Element (T.pack owner)) (T.pack title) i) >>= orDie
    ["todo",file,"done",key] -> using file $ \db ->
      commit db [Update "Task" (Element (T.pack key)) (M.singleton "status" "done")] >>= orDie
    ["todo",file,"delete",key] -> using file $ \db ->
      commit db [Delete "Task" (Element (T.pack key))] >>= orDie
    ["todo",file,"list"] -> using file $ \db -> do
      i <- snapshot db >>= orDie
      output <- orDie (rows i "Task" [("title",Path "Task" ["title"]),
        ("project",Path "Task" ["project","projectName"]),
        ("owner",Path "Task" ["owner","personName"]),("status",Path "Task" ["status"])])
      mapM_ (\(x,fields) -> T.putStrLn (elementName x <> "  " <>
        T.intercalate " | " [k <> "=" <> elementName v | (k,v) <- M.toList fields])) output
    ["demo",file] -> do
      i <- orDie todoInstance
      create file i
      f <- orDie (publicView (instanceSchema i))
      view <- orDie (delta f i)
      T.putStrLn "Delta: public cards from Task.title and Task.owner.personName"
      orDie (rows view "Card" [("caption",Path "Card" ["caption"]),("assignee",Path "Card" ["assignee"])] ) >>= emit
      (g,j) <- orDie spanExample
      l <- orDie (sigma g j)
      r <- orDie (piMigration g j)
      T.putStrLn "Sigma: connected components, with original element witnesses"
      emit (M.map (M.map (map (\(o,_,x) -> (o,x)))) (sigmaOrigins l))
      T.putStrLn "Pi: compatible Person/Department/Ticket families (equijoin)"
      emit (M.map (M.map (map (\((o,_),x) -> (o,x)) . M.toList)) (piFamilies r))
      T.putStrLn ("Saved " <> T.pack file <> ". Try: catdb todo FILE list")
    ["help"] -> putStrLn usage
    [] -> putStrLn usage
    _ -> die usage

usage :: String
usage = unlines
  [ "CatDB 0.1 - persistent finite functors, one local file"
  , "catdb demo FILE"
  , "catdb todo FILE init|list|add ID PROJECT OWNER TITLE|done ID|delete ID"
  , "catdb init FILE DOCUMENT.json | dump FILE | apply FILE EDITS.json"
  , "catdb path FILE OBJECT ID [ARROW ...] | rows FILE OBJECT [a.b ...]"
  , "catdb query FILE delta|sigma|pi MAPPING.json [NEW_FILE]"
  , "catdb migrate FILE delta|sigma|pi MAPPING.json"
  , "catdb copy FILE NEW_FILE | check FILE"
  , "New files must not exist. Each command opens, locks, commits and closes."
  ]
