{-# LANGUAGE DeriveAnyClass #-}
module CatDB.Internal where

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
  deriving newtype (IsString, FromJSON, ToJSON, FromJSONKey, ToJSONKey, Binary)
newtype Arrow = Arrow { arrowName :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString, FromJSON, ToJSON, FromJSONKey, ToJSONKey, Binary)
newtype Element = Element { elementName :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString, FromJSON, ToJSON, FromJSONKey, ToJSONKey, Binary)

-- Paths are written in traversal order. Path c [] is the identity at c.
data Path = Path { start :: Object, steps :: [Arrow] }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, Binary)
data ArrowDef = ArrowDef { arrow :: Arrow, source :: Object, target :: Object }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, Binary)
data Equation = Equation { left :: Path, right :: Path }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, Binary)
data SchemaDef = SchemaDef
  { objects :: [Object], arrows :: [ArrowDef], equations :: [Equation] }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, Binary)

data Error
  = DuplicateObject Object | DuplicateArrow Arrow | UnknownObject Object
  | UnknownArrow Arrow | CyclicSchema Object | InvalidPath Path
  | NonParallel Equation | SchemaTooLarge Int | MappingShape
  | MappingEquation Equation | SchemaMismatch
  | InstanceShape | FunctionDomain Arrow | MissingElement Object Element
  | ReferenceViolation Arrow Element Element | EquationViolation Equation Element
  | RowShape Object Element | DuplicateElement Object Element
  | NaturalShape | NaturalityViolation Arrow Element
  | QueryLimit Text Integer | InternalInvariant Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON)

data Schema = Schema
  { schemaDefinition :: SchemaDef
  , generators :: Map Arrow ArrowDef
  , normalForms :: Map Path Path
  , endpoints :: Map Path Object
  , schemaMorphisms :: [Path]
  } deriving (Eq, Show)

data InstanceDef = InstanceDef
  { sets :: !(Map Object (Set Element))
  , functions :: !(Map Arrow (Map Element Element))
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON, Binary)
data Instance = Instance { dataSchema :: Schema, dataDefinition :: InstanceDef }
  deriving (Eq, Show)

data MappingDef = MappingDef
  { onObjects :: Map Object Object, onArrows :: Map Arrow Path }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, Binary)
data Mapping = Mapping
  { mappingDomain :: Schema, mappingCodomain :: Schema, mappingDefinition :: MappingDef }
  deriving (Eq, Show)

type Components = Map Object (Map Element Element)
data Natural = Natural
  { transformationSource :: Instance, transformationTarget :: Instance, transformationComponents :: Components }
  deriving (Eq, Show)

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
