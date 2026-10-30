{-# LANGUAGE OverloadedStrings #-}
module Main where

import CatDB
import Data.Aeson (eitherDecodeFileStrict, withObject, (.:))
import Data.Aeson.Types (parseEither)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [file,revision] -> do
      value <- eitherDecodeFileStrict file >>= either die pure
      (schemas,instances) <- either die pure $ parseEither (withObject "Studio export" $ \v ->
        (,) <$> v .: "schemas" <*> v .: "instances") value
      (schemaKey,dat) <- maybe (die "Unknown revision") pure (M.lookup (InstanceRev (T.pack revision)) instances)
      def <- maybe (die "Unknown schema") pure (M.lookup (schemaKey :: SchemaKey) schemas)
      s <- either (die . show) pure (schema def)
      i <- either (die . show) pure (instanceOf s dat)
      mapM_ (\(o,xs) -> putStrLn (T.unpack (objectName o) ++ ": " ++ show (S.size xs) ++ " records")) (M.toList (sets (instanceData i)))
    _ -> die "studio-export-example DEFINITIONS.json REVISION\nReads and checks an exported revision through the public CatDB library."
