{-# LANGUAGE DeriveAnyClass #-}
module CatDB.System
  ( SystemMorphism, systemMorphism, systemMorphismWithBudget
  , systemMapping, systemSource, systemTarget, systemComponents
  , identitySystemMorphism, composeSystemMorphism, composeSystemMorphismWithBudget
  ) where

import CatDB.Budget
import CatDB.Internal
import CatDB.Schema (identityMapping, composeMapping)
import CatDB.Instance (natural, identityNatural, composeNatural)
import CatDB.Migration (deltaWithBudget, deltaNaturalWithBudget)
import Control.DeepSeq (NFData(..))

data SystemMorphism = SystemMorphism Mapping Instance Instance Natural
  deriving (Eq, Show)
instance NFData SystemMorphism where
  rnf (SystemMorphism f i j alpha) = rnf f `seq` rnf i `seq` rnf j `seq` rnf alpha

systemMorphism :: Mapping -> Instance -> Instance -> Components -> Either Error SystemMorphism
systemMorphism = systemMorphismWithBudget defaultBudget

systemMorphismWithBudget :: Budget -> Mapping -> Instance -> Instance -> Components -> Either Error SystemMorphism
systemMorphismWithBudget budget f i j cs = do
  ensure SchemaMismatch (instanceSchema i == domain f && instanceSchema j == codomain f)
  ensureInstanceBudget budget i
  pulled <- deltaWithBudget budget f j
  alpha <- natural i pulled cs
  pure (SystemMorphism f i j alpha)

systemMapping :: SystemMorphism -> Mapping
systemMapping (SystemMorphism f _ _ _) = f
systemSource, systemTarget :: SystemMorphism -> Instance
systemSource (SystemMorphism _ i _ _) = i
systemTarget (SystemMorphism _ _ j _) = j
systemComponents :: SystemMorphism -> Components
systemComponents (SystemMorphism _ _ _ alpha) = components alpha

identitySystemMorphism :: Instance -> SystemMorphism
identitySystemMorphism i = SystemMorphism (identityMapping (instanceSchema i)) i i (identityNatural i)

-- Traversal order, matching composeMapping and composeNatural.
composeSystemMorphism :: SystemMorphism -> SystemMorphism -> Either Error SystemMorphism
composeSystemMorphism = composeSystemMorphismWithBudget defaultBudget

composeSystemMorphismWithBudget :: Budget -> SystemMorphism -> SystemMorphism -> Either Error SystemMorphism
composeSystemMorphismWithBudget budget (SystemMorphism f i j alpha) (SystemMorphism g j' k beta) = do
  ensure SchemaMismatch (j == j')
  fg <- composeMapping f g
  pulledBeta <- deltaNaturalWithBudget budget f beta
  composed <- composeNatural alpha pulledBeta
  systemMorphismWithBudget budget fg i k (components composed)
