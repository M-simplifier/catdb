{-# LANGUAGE DeriveAnyClass #-}
module CatDB.Internal where

import CatDB.Budget
import Control.DeepSeq (NFData(..))
import Data.Aeson (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
import Data.Binary (Binary)
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.String (IsString)
import GHC.Generics (Generic)

newtype Object = Object { objectName :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString, FromJSON, ToJSON, FromJSONKey, ToJSONKey, Binary, NFData)
newtype Arrow = Arrow { arrowName :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString, FromJSON, ToJSON, FromJSONKey, ToJSONKey, Binary, NFData)
newtype Element = Element { elementName :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString, FromJSON, ToJSON, FromJSONKey, ToJSONKey, Binary, NFData)

-- Paths are written in traversal order. Path c [] is the identity at c.
data Path = Path { start :: Object, steps :: [Arrow] }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, Binary, NFData)
data ArrowDef = ArrowDef { arrow :: Arrow, source :: Object, target :: Object }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, Binary, NFData)
data Equation = Equation { left :: Path, right :: Path }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, Binary, NFData)
data SchemaDef = SchemaDef
  { objects :: [Object], arrows :: [ArrowDef], equations :: [Equation] }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, Binary, NFData)

data Error
  = DuplicateObject Object | DuplicateArrow Arrow | UnknownObject Object
  | UnknownArrow Arrow | CyclicSchema Object | InvalidPath Path
  | NonParallel Equation | SchemaTooLarge Int | MappingShape
  | MappingEquation Equation | SchemaMismatch
  | InstanceShape | FunctionDomain Arrow | MissingElement Object Element
  | ReferenceViolation Arrow Element Element | EquationViolation Equation Element
  | RowShape Object Element | DuplicateElement Object Element
  | NaturalShape | NaturalityViolation Arrow Element
  | QueryLimit Text Integer | InvalidBudget BudgetLimit Integer
  | BudgetExceeded BudgetLimit Integer Integer | InternalInvariant Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, NFData)

data Schema = Schema
  { schemaDefinition :: SchemaDef
  , generators :: Map Arrow ArrowDef
  , normalForms :: Map Path Path
  , endpoints :: Map Path Object
  , schemaMorphisms :: [Path]
  } deriving (Eq, Show)
instance NFData Schema where
  rnf (Schema def gs nf ends ms) = rnf def `seq` rnf gs `seq` rnf nf `seq` rnf ends `seq` rnf ms

data InstanceDef = InstanceDef
  { sets :: !(Map Object (Set Element))
  , functions :: !(Map Arrow (Map Element Element))
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, Binary, NFData)
data Instance = Instance { dataSchema :: Schema, dataDefinition :: InstanceDef }
  deriving (Eq, Show)
instance NFData Instance where
  rnf (Instance s dat) = rnf s `seq` rnf dat

data MappingDef = MappingDef
  { onObjects :: Map Object Object, onArrows :: Map Arrow Path }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, Binary, NFData)
data Mapping = Mapping
  { mappingDomain :: Schema, mappingCodomain :: Schema, mappingDefinition :: MappingDef }
  deriving (Show)
instance NFData Mapping where
  rnf (Mapping c d def) = rnf c `seq` rnf d `seq` rnf def

-- A functor sends generators to morphisms, not to chosen spellings of paths.
instance Eq Mapping where
  Mapping c d f == Mapping c' d' g =
    c == c' && d == d' && onObjects f == onObjects g
      && canonical d (onArrows f) == canonical d' (onArrows g)
    where canonical s = M.map (\p -> M.findWithDefault p p (normalForms s))

type Components = Map Object (Map Element Element)
data Natural = Natural
  { transformationSource :: Instance, transformationTarget :: Instance, transformationComponents :: Components }
  deriving (Eq, Show)
instance NFData Natural where
  rnf (Natural i j cs) = rnf i `seq` rnf j `seq` rnf cs

-- These public observations are ordinary functions rather than exported record
-- labels: a caller cannot use record update to bypass the smart constructors.
definition :: Schema -> SchemaDef
definition = schemaDefinition
morphisms :: Schema -> [Path]
morphisms = schemaMorphisms
instanceSchema :: Instance -> Schema
instanceSchema = dataSchema
instanceData :: Instance -> InstanceDef
instanceData = dataDefinition
domain, codomain :: Mapping -> Schema
domain = mappingDomain
codomain = mappingCodomain
mappingDef :: Mapping -> MappingDef
mappingDef = mappingDefinition
naturalSource, naturalTarget :: Natural -> Instance
naturalSource = transformationSource
naturalTarget = transformationTarget
components :: Natural -> Components
components = transformationComponents

lookupE :: Ord k => Error -> k -> Map k a -> Either Error a
lookupE e k = maybe (Left e) Right . M.lookup k

ensure :: Error -> Bool -> Either Error ()
ensure e ok = if ok then Right () else Left e

-- Finite equivalence closure. The minimum member names each class; names do not
-- affect the quotient. No rewrite orientation or proof-search timeout is used.
quotient :: Ord a => [a] -> [(a,a)] -> Map a a
quotient universe pairs = M.map (root parents) parents
  where
    initial = M.fromList [(x,x) | x <- universe]
    root m x = case M.lookup x m of
      Just y | y /= x -> root m y
      _ -> x
    parents = foldl' join initial pairs
    join m (x,y) = let a = root m x; b = root m y
                  in if a == b then m else M.insert (max a b) (min a b) m

objectSet :: Schema -> Set Object
objectSet = S.fromList . objects . definition

elements :: Instance -> Object -> Either Error (Set Element)
elements i o = lookupE (UnknownObject o) o (sets (instanceData i))

ensureBudget :: Budget -> BudgetLimit -> Integer -> Either Error ()
ensureBudget b kind used = do
  let allowed = budgetAllowance b kind
  ensure (InvalidBudget kind allowed) (allowed >= 0)
  ensure (BudgetExceeded kind used allowed) (used <= allowed)

ensureSchemaBudget :: Budget -> Schema -> Either Error ()
ensureSchemaBudget b s = do
  ensureBudget b SchemaObjects (toInteger (length (objects (definition s))))
  ensureBudget b SchemaArrows (toInteger (length (arrows (definition s))))
  ensureBudget b SchemaPaths (toInteger (M.size (normalForms s)))
  ensureBudget b SchemaEquations (toInteger (length (equations (definition s))))
  ensureBudget b EquationChecks (schemaEquationChecks (definition s) (M.keys (normalForms s)))

ensureInstanceBudget :: Budget -> Instance -> Either Error ()
ensureInstanceBudget b i = do
  ensureSchemaBudget b (instanceSchema i)
  ensureBudget b InstanceElements (sum (map (toInteger . S.size) (M.elems (sets (instanceData i)))))
  ensureBudget b InstanceCells (sum (map (toInteger . M.size) (M.elems (functions (instanceData i)))))
  ensureBudget b EquationChecks (instanceEquationChecks (instanceSchema i) (instanceData i))

-- These are the same preflight charges used by schema/instance construction.
schemaEquationChecks :: SchemaDef -> [Path] -> Integer
schemaEquationChecks def ps =
  sum [pn * en * max 0 (n-m+1) * (m+1)
      | (n,pn) <- M.toList pathLengths,(m,en) <- M.toList equationLengths]
  where
    pathLengths = M.fromListWith (+) [(toInteger (length (steps p)),1::Integer) | p <- ps]
    equationLengths = M.fromListWith (+) [(toInteger (length (steps (left eq))),1::Integer) | eq <- equations def]

instanceEquationChecks :: Schema -> InstanceDef -> Integer
instanceEquationChecks s dat =
  sum [toInteger (S.size (M.findWithDefault S.empty (start (left eq)) (sets dat)))
       * toInteger (1 + length (steps (left eq)) + length (steps (right eq)))
      | eq <- equations (definition s)]
