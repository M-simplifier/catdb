{-# LANGUAGE DeriveAnyClass #-}
module CatDB.Budget
  ( Budget(..), BudgetLimit(..), defaultBudget, budgetAllowance ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)

data BudgetLimit
  = SchemaObjects | SchemaArrows | SchemaPaths | SchemaEquations | EquationChecks | InstanceElements | InstanceCells
  | SigmaTokens | PiBindings | PiFamilies | CatalogRecords | CatalogReplays | WitnessNodes
  deriving (Eq, Ord, Show, Generic, NFData, FromJSON, ToJSON)

data Budget = Budget
  { maxSchemaObjects :: Integer, maxSchemaArrows :: Integer, maxSchemaPaths :: Integer
  , maxSchemaEquations :: Integer, maxEquationChecks :: Integer
  , maxInstanceElements :: Integer, maxInstanceCells :: Integer
  , maxSigmaTokens :: Integer, maxPiBindings :: Integer, maxPiFamilies :: Integer
  , maxCatalogRecords :: Integer, maxCatalogReplays :: Integer, maxWitnessNodes :: Integer
  } deriving (Eq, Show, Generic, NFData, FromJSON, ToJSON)

defaultBudget :: Budget
defaultBudget = Budget 256 1024 4096 1024 1000000 2000000 4000000 100000 1000000 100000 200000 256 1000000

budgetAllowance :: Budget -> BudgetLimit -> Integer
budgetAllowance b k = case k of
  SchemaObjects -> maxSchemaObjects b
  SchemaArrows -> maxSchemaArrows b
  SchemaPaths -> maxSchemaPaths b
  SchemaEquations -> maxSchemaEquations b
  EquationChecks -> maxEquationChecks b
  InstanceElements -> maxInstanceElements b
  InstanceCells -> maxInstanceCells b
  SigmaTokens -> maxSigmaTokens b
  PiBindings -> maxPiBindings b
  PiFamilies -> maxPiFamilies b
  CatalogRecords -> maxCatalogRecords b
  CatalogReplays -> maxCatalogReplays b
  WitnessNodes -> maxWitnessNodes b
