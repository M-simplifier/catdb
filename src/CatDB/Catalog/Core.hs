{-# LANGUAGE DeriveAnyClass #-}
module CatDB.Catalog.Core where

import CatDB.Budget
import CatDB.Internal
import CatDB.Schema
import CatDB.Instance
import CatDB.Migration
import CatDB.System
import Control.DeepSeq (NFData(..))
import Control.Monad (forM, forM_, unless)
import Data.Aeson (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

newtype SchemaKey = SchemaKey { unSchemaKey :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString, FromJSON, ToJSON, FromJSONKey, ToJSONKey, NFData)
newtype InstanceRev = InstanceRev { unInstanceRev :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString, FromJSON, ToJSON, FromJSONKey, ToJSONKey, NFData)
newtype MappingKey = MappingKey { unMappingKey :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString, FromJSON, ToJSON, FromJSONKey, ToJSONKey, NFData)
newtype CorrespondenceKey = CorrespondenceKey { unCorrespondenceKey :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString, FromJSON, ToJSON, FromJSONKey, ToJSONKey, NFData)
newtype DerivationKey = DerivationKey { unDerivationKey :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString, FromJSON, ToJSON, FromJSONKey, ToJSONKey, NFData)
newtype SystemKey = SystemKey { unSystemKey :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString, FromJSON, ToJSON, FromJSONKey, ToJSONKey, NFData)
newtype ViewKey = ViewKey { unViewKey :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString, FromJSON, ToJSON, FromJSONKey, ToJSONKey, NFData)
newtype AnchorId = AnchorId { unAnchorId :: Text }
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (IsString, FromJSON, ToJSON, FromJSONKey, ToJSONKey, NFData)

data CatalogError = NativeError Error | CatalogInvalid Text | UnknownCatalogKey Text Text
  | CatalogKeyExists Text Text
  deriving (Eq, Show, Generic, NFData, ToJSON)

data RowRef = RowRef
  { rowRevision :: InstanceRev, rowObject :: Object, rowElement :: Element }
  deriving (Eq, Ord, Show, Generic, NFData, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

data Transport = Transport { transportPath :: Path, transportRow :: RowRef }
  deriving (Eq, Ord, Show, Generic, NFData, FromJSON, ToJSON)
data FamilyMember = FamilyMember { memberObject :: Object, memberPath :: Path, memberRow :: RowRef }
  deriving (Eq, Ord, Show, Generic, NFData, FromJSON, ToJSON)
data Witness = PullRef RowRef | SigmaClass [Transport] | PiFamily [FamilyMember] | GeneratedRef RowRef
  deriving (Eq, Show, Generic, NFData, FromJSON, ToJSON)
data DerivationSpec = PullAlong MappingKey | PushAlong MappingKey | JoinAlong MappingKey
  | Generate (Map Object (Set Element))
  deriving (Eq, Show, Generic, NFData, FromJSON, ToJSON)
data Derivation = Derivation
  { derivationInput :: InstanceRev, derivationOutput :: InstanceRev
  , derivationSpec :: DerivationSpec, derivationWitnesses :: Map RowRef Witness }
  deriving (Eq, Show, Generic, NFData, FromJSON, ToJSON)
data SourceBinding = Pinned InstanceRev | Active SystemKey
  deriving (Eq, Show, Generic, NFData, FromJSON, ToJSON)
data ViewAdoption = ViewAdoption
  { adoptionBinding :: SourceBinding, adoptionDerivation :: DerivationKey
  , adoptionSourceRevision :: InstanceRev }
  deriving (Eq, Show, Generic, NFData, FromJSON, ToJSON)
data ViewFreshness = Current | PinnedSnapshot | Stale InstanceRev InstanceRev
  deriving (Eq, Show, Generic, NFData, FromJSON, ToJSON)
data RevisionNote = RevisionNote
  { revisionPrevious :: Maybe InstanceRev, revisionReason :: Text, revisionAnchors :: [AnchorId] }
  deriving (Eq, Show, Generic, NFData, FromJSON, ToJSON)
data ExplainedRow = ExplainedRow
  { explainedRow :: RowRef, directAnchors :: [AnchorId]
  , rowDerivation :: Maybe (DerivationKey,Witness) }
  deriving (Eq, Show, Generic, NFData, ToJSON)

data Catalog = Catalog
  { cRevision :: !Integer
  , cSchemas :: !(Map SchemaKey Schema)
  , cInstances :: !(Map InstanceRev (SchemaKey,Instance))
  , cMappings :: !(Map MappingKey (SchemaKey,SchemaKey,Mapping))
  , cCorrespondences :: !(Map CorrespondenceKey (InstanceRev,InstanceRev,MappingKey,Components))
  , cDerivations :: !(Map DerivationKey Derivation)
  , cReplayBindings :: !(Map DerivationKey Integer)
  , cBindings :: !(Map RowRef (Set AnchorId))
  , cHeads :: !(Map SystemKey InstanceRev)
  , cViews :: !(Map ViewKey ViewAdoption)
  , cNotes :: !(Map InstanceRev RevisionNote)
  } deriving (Eq, Show)
instance NFData Catalog where
  rnf c = rnf (cRevision c) `seq` rnf (cSchemas c) `seq` rnf (cInstances c)
    `seq` rnf (cMappings c) `seq` rnf (cCorrespondences c) `seq` rnf (cDerivations c)
    `seq` rnf (cReplayBindings c) `seq` rnf (cBindings c) `seq` rnf (cHeads c)
    `seq` rnf (cViews c) `seq` rnf (cNotes c)

emptyCatalog :: Catalog
emptyCatalog = Catalog
  { cRevision=0, cSchemas=M.empty, cInstances=M.empty, cMappings=M.empty
  , cCorrespondences=M.empty, cDerivations=M.empty, cReplayBindings=M.empty
  , cBindings=M.empty, cHeads=M.empty, cViews=M.empty, cNotes=M.empty }


catalogRevision :: Catalog -> Integer
catalogRevision = cRevision
catalogSchemas :: Catalog -> Map SchemaKey SchemaDef
catalogSchemas = M.map definition . cSchemas
catalogInstances :: Catalog -> Map InstanceRev (SchemaKey,InstanceDef)
catalogInstances = M.map (\(k,i) -> (k,instanceData i)) . cInstances
catalogMappings :: Catalog -> Map MappingKey (SchemaKey,SchemaKey,MappingDef)
catalogMappings = M.map (\(a,b,f) -> (a,b,mappingDef f)) . cMappings
catalogCorrespondences :: Catalog -> Map CorrespondenceKey (InstanceRev,InstanceRev,MappingKey,Components)
catalogCorrespondences = cCorrespondences
catalogDerivations :: Catalog -> Map DerivationKey Derivation
catalogDerivations = cDerivations
catalogHeads :: Catalog -> Map SystemKey InstanceRev
catalogHeads = cHeads
catalogViews :: Catalog -> Map ViewKey ViewAdoption
catalogViews = cViews
catalogRevisionNotes :: Catalog -> Map InstanceRev RevisionNote
catalogRevisionNotes = cNotes
catalogBindings :: Catalog -> Map RowRef (Set AnchorId)
catalogBindings = cBindings

native :: Either Error a -> Either CatalogError a
native = either (Left . NativeError) Right
invalid :: Text -> Bool -> Either CatalogError ()
invalid message condition = unless condition (Left (CatalogInvalid message))
findKey :: Ord k => Text -> Text -> k -> Map k a -> Either CatalogError a
findKey kind name key = maybe (Left (UnknownCatalogKey kind name)) Right . M.lookup key
newKey :: Ord k => Text -> Text -> k -> Map k a -> Either CatalogError ()
newKey kind name key table = do
  invalid "catalog keys must contain 1–200 characters" (not (T.null name) && T.length name <= 200)
  unless (M.notMember key table) (Left (CatalogKeyExists kind name))
bump :: Catalog -> Catalog
bump c = c { cRevision = cRevision c + 1 }

lookupSchema :: SchemaKey -> Catalog -> Either CatalogError Schema
lookupSchema k = findKey "schema" (unSchemaKey k) k . cSchemas
lookupInstance :: InstanceRev -> Catalog -> Either CatalogError Instance
lookupInstance k c = snd <$> findKey "instance" (unInstanceRev k) k (cInstances c)
lookupMapping :: MappingKey -> Catalog -> Either CatalogError Mapping
lookupMapping k c = (\(_,_,f) -> f) <$> findKey "mapping" (unMappingKey k) k (cMappings c)
lookupDerivation :: DerivationKey -> Catalog -> Either CatalogError Derivation
lookupDerivation k = findKey "derivation" (unDerivationKey k) k . cDerivations

recordSchema :: Budget -> SchemaKey -> SchemaDef -> Catalog -> Either CatalogError Catalog
recordSchema b k def c = do
  newKey "schema" (unSchemaKey k) k (cSchemas c)
  s <- native (schemaWithBudget b def)
  pure (bump c {cSchemas=M.insert k s (cSchemas c)})
recordInstance :: Budget -> InstanceRev -> SchemaKey -> InstanceDef -> Catalog -> Either CatalogError Catalog
recordInstance b k sk dat c = do
  newKey "instance" (unInstanceRev k) k (cInstances c)
  s <- lookupSchema sk c
  i <- native (instanceOfWithBudget b s dat)
  pure (bump c {cInstances=M.insert k (sk,i) (cInstances c)})
recordMapping :: Budget -> MappingKey -> SchemaKey -> SchemaKey -> MappingDef -> Catalog -> Either CatalogError Catalog
recordMapping budget k a b def c = do
  newKey "mapping" (unMappingKey k) k (cMappings c)
  from <- lookupSchema a c
  to <- lookupSchema b c
  native (ensureSchemaBudget budget from)
  native (ensureSchemaBudget budget to)
  f <- native (mapping from to def)
  pure (bump c {cMappings=M.insert k (a,b,f) (cMappings c)})
recordCorrespondence :: Budget -> CorrespondenceKey -> InstanceRev -> InstanceRev -> MappingKey -> Components -> Catalog -> Either CatalogError Catalog
recordCorrespondence b k from to mk cs c = do
  newKey "correspondence" (unCorrespondenceKey k) k (cCorrespondences c)
  (fromSchema,i) <- findKey "instance" (unInstanceRev from) from (cInstances c)
  (toSchema,j) <- findKey "instance" (unInstanceRev to) to (cInstances c)
  (mappingSource,mappingTarget,f) <- findKey "mapping" (unMappingKey mk) mk (cMappings c)
  invalid "correspondence schema revision mismatch" (fromSchema == mappingSource && toSchema == mappingTarget)
  native (ensureBudget b InstanceCells (sum (map (toInteger . M.size) (M.elems cs))))
  _ <- native (systemMorphismWithBudget b f i j cs)
  pure (bump c {cCorrespondences=M.insert k (from,to,mk,cs) (cCorrespondences c)})

validRow :: RowRef -> Catalog -> Either CatalogError ()
validRow (RowRef rev o x) c = do
  i <- lookupInstance rev c
  xs <- native (elements i o)
  native (ensure (MissingElement o x) (x `S.member` xs))

bindAnchor :: RowRef -> AnchorId -> Catalog -> Either CatalogError Catalog
bindAnchor row anchor c = do
  validRow row c
  invalid "an anchor ID must be nonempty" (not (T.null (unAnchorId anchor)))
  invalid "derived rows retain their derivation; bind anchors to source rows" (all ((/= rowRevision row) . derivationOutput) (M.elems (cDerivations c)))
  pure (bump c {cBindings=M.insertWith S.union row (S.singleton anchor) (cBindings c)})

witnessInputs :: Witness -> [RowRef]
witnessInputs w = case w of
  PullRef r -> [r]
  SigmaClass ts -> map transportRow ts
  PiFamily ms -> map memberRow ms
  GeneratedRef r -> [r]
witnessSize :: Witness -> Integer
witnessSize = (1+) . toInteger . length . witnessInputs

computeDerivation :: Budget -> InstanceRev -> DerivationSpec -> InstanceRev -> Catalog
  -> Either CatalogError (SchemaKey,Instance,Map RowRef Witness,Integer)
computeDerivation b out spec sourceRev c = do
  (sourceKey,i) <- findKey "instance" (unInstanceRev sourceRev) sourceRev (cInstances c)
  native (ensureInstanceBudget b i)
  let src o x = RowRef sourceRev o x
      dst o x = RowRef out o x
      direct constructor result = M.fromList [(dst o x,constructor (src o x))
        | (o,xs) <- M.toList (sets (instanceData result)),x <- S.toList xs]
  (sk,result,ws,bindings) <- case spec of
    Generate seeds -> do
      r <- native (generated seeds i)
      pure (sourceKey,r,direct GeneratedRef r,0)
    _ -> do
      let mk = case spec of PullAlong k -> k; PushAlong k -> k; JoinAlong k -> k
      (a,z,f) <- findKey "mapping" (unMappingKey mk) mk (cMappings c)
      invalid "derivation input schema revision mismatch" (sourceKey == case spec of PullAlong _ -> z; _ -> a)
      case spec of
        PullAlong _ -> do
          r <- native (deltaWithBudget b f i)
          pairs <- forM (M.toList (sets (instanceData r))) $ \(o,xs) -> do
            fo <- native (mapObject f o)
            pure [(dst o x,PullRef (src fo x)) | x <- S.toList xs]
          pure (a,r,M.fromList (concat pairs),0)
        PushAlong _ -> do
          l <- native (sigmaWithBudget b f i)
          let ws' = M.fromList [(dst o x,SigmaClass [Transport p (src origin value) | (origin,p,value) <- tokens])
                | (o,rows') <- M.toList (sigmaOrigins l),(x,tokens) <- M.toList rows']
          pure (z,sigmaInstance l,ws',0)
        JoinAlong _ -> do
          (r,statistics) <- native (piMigrationWithStatistics b f i)
          let ws' = M.fromList [(dst o x,PiFamily [FamilyMember origin p (src origin value) | ((origin,p),value) <- M.toList family])
                | (o,rows') <- M.toList (piFamilies r),(x,family) <- M.toList rows']
          pure (z,piInstance r,ws',piExploredBindings statistics)
  native (ensureBudget b WitnessNodes (sum (map witnessSize (M.elems ws))))
  pure (sk,result,ws,bindings)

derive :: Budget -> DerivationKey -> InstanceRev -> DerivationSpec -> InstanceRev -> Catalog -> Either CatalogError Catalog
derive b k out spec from c = do
  newKey "derivation" (unDerivationKey k) k (cDerivations c)
  newKey "instance" (unInstanceRev out) out (cInstances c)
  native (ensureBudget b CatalogReplays (toInteger (M.size (cDerivations c)) + 1))
  (sk,result,ws,bindings) <- computeDerivation b out spec from c
  let storedSpec = case spec of
        Generate seeds -> Generate (M.filter (not . S.null) seeds)
        _ -> spec
      d = Derivation from out storedSpec ws
      next = bump c {cInstances=M.insert out (sk,result) (cInstances c)
                    ,cDerivations=M.insert k d (cDerivations c)
                    ,cReplayBindings=M.insert k bindings (cReplayBindings c)}
  checkReplayBudget b next
  pure next

explainRow :: RowRef -> Catalog -> Either CatalogError ExplainedRow
explainRow row c = do
  validRow row c
  let derived = [(k,w) | (k,d) <- M.toList (cDerivations c), Just w <- [M.lookup row (derivationWitnesses d)]]
  case derived of
    [] -> pure (ExplainedRow row (S.toList (M.findWithDefault S.empty row (cBindings c))) Nothing)
    [one] -> pure (ExplainedRow row [] (Just one))
    _ -> Left (CatalogInvalid "a result row has multiple derivations")

derivationLineage :: Budget -> DerivationKey -> Catalog -> Either CatalogError [InstanceRev]
derivationLineage b key c = lookupDerivation key c >>= go S.empty 0 . derivationInput
  where
    outputs = M.fromList [(derivationOutput d,d) | d <- M.elems (cDerivations c)]
    go seen n rev = do
      native (ensureBudget b CatalogReplays n)
      invalid "cyclic derivation lineage" (rev `S.notMember` seen)
      case M.lookup rev outputs of
        Nothing -> pure [rev]
        Just d -> (rev:) <$> go (S.insert rev seen) (n+1) (derivationInput d)

rootInput :: Budget -> DerivationKey -> Catalog -> Either CatalogError InstanceRev
rootInput b key c = do
  lineage <- derivationLineage b key c
  case reverse lineage of
    root:_ -> pure root
    [] -> Left (CatalogInvalid "empty derivation lineage")

adoptSystem :: SystemKey -> InstanceRev -> Catalog -> Either CatalogError Catalog
adoptSystem k rev c = do
  _ <- lookupInstance rev c
  invalid "system key must be nonempty" (not (T.null (unSystemKey k)))
  pure (bump c {cHeads=M.insert k rev (cHeads c)})

adoptView :: Budget -> ViewKey -> SourceBinding -> DerivationKey -> Catalog -> Either CatalogError Catalog
adoptView b k binding dk c = do
  invalid "view key must be nonempty" (not (T.null (unViewKey k)))
  lineage <- derivationLineage b dk c
  expected <- case binding of
    Pinned rev -> lookupInstance rev c >> pure rev
    Active sk -> findKey "head" (unSystemKey sk) sk (cHeads c)
  invalid "the selected source revision is outside the derivation lineage" (expected `elem` lineage)
  pure (bump c {cViews=M.insert k (ViewAdoption binding dk expected) (cViews c)})

viewFreshness :: Budget -> ViewKey -> Catalog -> Either CatalogError ViewFreshness
viewFreshness b k c = do
  ViewAdoption binding dk captured <- findKey "view" (unViewKey k) k (cViews c)
  lineage <- derivationLineage b dk c
  invalid "view source revision is outside the derivation lineage" (captured `elem` lineage)
  case binding of
    Pinned rev -> invalid "pinned source mismatch" (captured == rev) >> pure PinnedSnapshot
    Active sk -> do
      current <- findKey "head" (unSystemKey sk) sk (cHeads c)
      pure (if captured == current then Current else Stale captured current)

recordRevisionNote :: InstanceRev -> RevisionNote -> Catalog -> Either CatalogError Catalog
recordRevisionNote rev note c = do
  _ <- lookupInstance rev c
  forM_ (revisionPrevious note) (\old -> lookupInstance old c >> invalid "a revision cannot precede itself" (old /= rev))
  newKey "revision note" (unInstanceRev rev) rev (cNotes c)
  let next = bump c {cNotes=M.insert rev note (cNotes c)}
  validateRevisionHistory next
  pure next

-- Readmission is structural work only: never execute a Kan extension here.
-- Replay bindings are private certificates computed by derive or replayCatalog.
checkReplayBudget :: Budget -> Catalog -> Either CatalogError ()
checkReplayBudget b c = checkReplayFootprint b c >> checkCachedPiBudget b c

checkReplayFootprint :: Budget -> Catalog -> Either CatalogError ()
checkReplayFootprint b c = do
  invalid "negative catalog revision" (cRevision c >= 0)
  forM_ (M.elems (cSchemas c)) (native . ensureSchemaBudget b)
  forM_ (M.elems (cInstances c)) (native . ensureInstanceBudget b . snd)
  native (ensureBudget b CatalogReplays (toInteger (M.size (cDerivations c))))
  native (ensureBudget b WitnessNodes (sum [witnessSize w | d <- M.elems (cDerivations c),w <- M.elems (derivationWitnesses d)]))
  forM_ (M.elems (cCorrespondences c)) $ \(from,to,mk,cs) -> do
    i <- lookupInstance from c
    j <- lookupInstance to c
    f <- lookupMapping mk c
    native (ensureInstanceBudget b i)
    native (ensureBudget b InstanceCells (sum (map (toInteger . M.size) (M.elems cs))))
    checkDeltaFootprint b f j
  forM_ (M.elems (cDerivations c)) $ \d -> case derivationSpec d of
    PushAlong _ -> native (ensureBudget b SigmaTokens
      (sum [toInteger (length ts) | SigmaClass ts <- M.elems (derivationWitnesses d)]))
    JoinAlong _ -> do
      output <- lookupInstance (derivationOutput d) c
      native (ensureBudget b PiFamilies (sum (map (toInteger . S.size) (M.elems (sets (instanceData output))))))
    _ -> pure ()

checkCachedPiBudget :: Budget -> Catalog -> Either CatalogError ()
checkCachedPiBudget b c = do
  invalid "missing replay admission certificate" (M.keysSet (cReplayBindings c) == M.keysSet (cDerivations c))
  forM_ (M.toList (cDerivations c)) $ \(dk,d) -> do
    bindings <- findKey "replay certificate" (unDerivationKey dk) dk (cReplayBindings c)
    invalid "negative replay admission certificate" (bindings >= 0)
    case derivationSpec d of
      JoinAlong _ -> native (ensureBudget b PiBindings bindings)
      _ -> invalid "non-Pi derivation has a Pi admission cost" (bindings == 0)

checkDeltaFootprint :: Budget -> Mapping -> Instance -> Either CatalogError ()
checkDeltaFootprint b f j = do
  native (ensureSchemaBudget b (domain f))
  native (ensureInstanceBudget b j)
  sizes <- M.fromList <$> forM (objects (definition (domain f))) (\o -> do
    fo <- native (mapObject f o)
    xs <- native (elements j fo)
    pure (o,toInteger (S.size xs)))
  let count o = M.findWithDefault 0 o sizes
  native (ensureBudget b InstanceElements (sum (M.elems sizes)))
  native (ensureBudget b InstanceCells (sum [count (source a) | a <- arrows (definition (domain f))]))
  native (ensureBudget b EquationChecks (sum [count (start (left eq))
    * toInteger (1 + length (steps (left eq)) + length (steps (right eq)))
    | eq <- equations (definition (domain f))]))

validateCatalog :: Budget -> Catalog -> Either CatalogError ()
validateCatalog b c = () <$ replayCatalog b c

-- The native decoder is the only path besides derive that creates certificates.
replayCatalog :: Budget -> Catalog -> Either CatalogError Catalog
replayCatalog b c = do
  checkReplayFootprint b c
  validateRevisionHistory c
  let outputs = map derivationOutput (M.elems (cDerivations c))
  invalid "a revision is the output of multiple derivations" (length outputs == S.size (S.fromList outputs))
  costs <- forM (M.toList (cDerivations c)) $ \(dk,d) -> do
    _ <- rootInput b dk c
    (sk,result,ws,bindings) <- computeDerivation b (derivationOutput d) (derivationSpec d) (derivationInput d) c
    (savedKey,saved) <- findKey "instance" (unInstanceRev (derivationOutput d)) (derivationOutput d) (cInstances c)
    invalid "derived instance or witness does not reproduce" (sk == savedKey && result == saved && ws == derivationWitnesses d)
    pure (dk,bindings)
  forM_ (M.toList (cBindings c)) $ \(row,anchors) -> do
    validRow row c
    invalid "derived rows cannot carry direct anchor bindings" (rowRevision row `notElem` outputs)
    invalid "empty root binding" (not (S.null anchors))
  forM_ (M.elems (cHeads c)) (\rev -> lookupInstance rev c >> pure ())
  forM_ (M.keys (cViews c)) (\key -> viewFreshness b key c >> pure ())
  forM_ (M.toList (cNotes c)) $ \(rev,note) -> do
    _ <- lookupInstance rev c
    forM_ (revisionPrevious note) (\old -> lookupInstance old c >> invalid "a revision cannot precede itself" (old /= rev))
  let admitted = c {cReplayBindings=M.fromList costs}
  checkCachedPiBudget b admitted
  pure admitted

validateRevisionHistory :: Catalog -> Either CatalogError ()
validateRevisionHistory c = forM_ (M.keys (cNotes c)) (walk S.empty)
  where
    walk seen rev = do
      invalid "cyclic temporal revision history" (rev `S.notMember` seen)
      case M.lookup rev (cNotes c) >>= revisionPrevious of
        Nothing -> pure ()
        Just previous -> walk (S.insert rev seen) previous
