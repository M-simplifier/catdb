{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import CatDB
import Control.Exception (bracket)
import qualified Data.Map.Strict as M
import System.Environment (getArgs)

must :: Show e => Either e a -> IO a
must = either (fail . show) pure

main :: IO ()
main = do
  args <- getArgs
  path <- case args of [p] -> pure p; _ -> fail "embedded-example NEW_FILE.catdb"
  s <- must $ schema $ SchemaDef ["Task","Text"] [ArrowDef "title" "Task" "Text"] []
  initial <- must $ edit [Insert "Text" "Ship a useful experiment" M.empty,
    Insert "Task" "first" (M.singleton "title" "Ship a useful experiment")] (empty s)
  bracket (createDatabase path initial >>= must) (\db -> closeDatabase db >>= must) $ \db -> do
    transaction db (\i -> do
      next <- edit [Insert "Text" "Keep the relationships" M.empty,
                    Update "Task" "first" (M.singleton "title" "Keep the relationships")] i
      pure ((),next)) >>= must
    i <- snapshot db >>= must
    must (rows i "Task" [("title",Path "Task" ["title"])]) >>= print
