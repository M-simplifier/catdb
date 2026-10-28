module NativeMemory (nativeMemoryTests, nativeFixture) where

import CatDB
import Control.DeepSeq (force)
import qualified Control.Exception as E
import Control.Monad (foldM, unless)
import Data.Either (isLeft)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

ok :: Show e => Either e a -> IO a
ok = either (fail . show) pure
check :: String -> Bool -> IO ()
check label value = unless value (fail ("native memory: " ++ label))

sourceDef :: SchemaDef
sourceDef = SchemaDef ["Clue","Pattern","Action"]
  [ArrowDef "cp" "Clue" "Pattern",ArrowDef "ap" "Action" "Pattern"] []
queryDef :: Bool -> SchemaDef
queryDef withEquation = SchemaDef (objects sourceDef ++ ["Query"])
  (arrows sourceDef ++ [ArrowDef "qc" "Query" "Clue",ArrowDef "qa" "Query" "Action"])
  [Equation (Path "Query" ["qc","cp"]) (Path "Query" ["qa","ap"]) | withEquation]
sourceData :: Bool -> InstanceDef
sourceData revised = InstanceDef
  (M.fromList [("Clue",S.fromList ["r1","r2"]),("Pattern",S.fromList ["quiet","friction"]),("Action",S.fromList ["a1","a2"])])
  (M.fromList [("cp",M.fromList [("r1",if revised then "friction" else "quiet"),("r2","friction")]),("ap",M.fromList [("a1","quiet"),("a2","friction")])])
inclusionDef :: SchemaDef -> MappingDef
inclusionDef def = MappingDef (M.fromList [(o,o) | o <- objects def])
  (M.fromList [(arrow a,Path (source a) [arrow a]) | a <- arrows def])

nativeFixture :: Either CatalogError Catalog
nativeFixture = do
  let b = defaultBudget
      v = SchemaDef ["Candidate","Action"] [ArrowDef "pick" "Candidate" "Action"] []
      r = SchemaDef ["Result"] [] []
  c0 <- recordSchema b "source" sourceDef emptyCatalog
  c1 <- recordSchema b "query" (queryDef True) c0
  c2 <- recordSchema b "view" v c1
  c3 <- recordSchema b "result" r c2
  c4 <- recordInstance b "base0" "source" (sourceData False) c3
  c5 <- recordMapping b "include" "source" "query" (inclusionDef sourceDef) c4
  c6 <- recordMapping b "read-view" "view" "query" (MappingDef (M.fromList [("Candidate","Query"),("Action","Action")]) (M.singleton "pick" (Path "Query" ["qa"]))) c5
  c7 <- recordMapping b "group-action" "view" "result" (MappingDef (M.fromList [("Candidate","Result"),("Action","Result")]) (M.singleton "pick" (identity "Result"))) c6
  c8 <- bindAnchor (RowRef "base0" "Clue" "r1") "experience:rain" c7
  c9 <- bindAnchor (RowRef "base0" "Clue" "r2") "experience:friction" c8
  c10 <- bindAnchor (RowRef "base0" "Action" "a1") "proposal:listen" c9
  c11 <- bindAnchor (RowRef "base0" "Action" "a2") "proposal:repair" c10
  c12 <- derive b "join" "joined" (JoinAlong "include") "base0" c11
  d <- lookupDerivation "join" c12
  let roots = S.fromList [rowElement row | (row,PiFamily members) <- M.toList (derivationWitnesses d),rowObject row == "Query",any ((==RowRef "base0" "Clue" "r1") . memberRow) members]
  c13 <- derive b "roots" "rooted" (Generate (M.singleton "Query" roots)) "joined" c12
  c14 <- derive b "projection" "projected" (PullAlong "read-view") "rooted" c13
  c15 <- derive b "group" "grouped" (PushAlong "group-action") "projected" c14
  c16 <- adoptSystem "perception" "base0" c15
  c17 <- adoptView b "current" (Active "perception") "group" c16
  adoptView b "pinned" (Pinned "base0") "group" c17

nativeMemoryTests :: FilePath -> IO ()
nativeMemoryTests dir = do
  createDirectoryIfMissing True dir
  let b = defaultBudget
  c <- ok nativeFixture >>= E.evaluate . force
  sourceI <- ok (lookupInstance "base0" c)
  joined <- ok (lookupInstance "joined" c)
  check "equation gives exactly two compatible candidate rows" (fmap S.size (elements joined "Query") == Right 2)
  cNoEquation <- ok (recordSchema b "query-no-equation" (queryDef False) c >>= recordMapping b "include-no-equation" "source" "query-no-equation" (inclusionDef sourceDef) >>= derive b "cross" "crossed" (JoinAlong "include-no-equation") "base0")
  crossed <- ok (lookupInstance "crossed" cNoEquation)
  check "removing only the equation gives four candidate rows" (fmap S.size (elements crossed "Query") == Right 4)
  f <- ok (lookupMapping "include" c)
  d0 <- ok (lookupSchema "query-no-equation" cNoEquation)
  check "a functor must preserve equations even if a sample happens to agree" (isLeft (mapping (codomain f) d0 (inclusionDef (queryDef True))))
  d <- ok (lookupDerivation "join" c)
  let queryFamilies = [members | (row,PiFamily members) <- M.toList (derivationWitnesses d),rowObject row == "Query"]
  check "each matching family has one shared Pattern slot" (all ((==3) . length) queryFamilies)
  grouped <- ok (lookupInstance "grouped" c)
  check "Pi then roots then Delta then Sigma yields one action group" (fmap S.size (elements grouped "Result") == Right 1)
  groupDerivation <- ok (lookupDerivation "group" c)
  check "Sigma retains tokens that point to the preceding revision" (all (\w -> case w of SigmaClass ts -> not (null ts) && all ((=="projected") . rowRevision . transportRow) ts; _ -> False) (M.elems (derivationWitnesses groupDerivation)))
  check "new result cannot be relabelled a direct experience" (isLeft (case M.keys (derivationWitnesses groupDerivation) of r:_ -> bindAnchor r "fake-fact" c; [] -> Left (CatalogInvalid "missing fixture")))
  equationsAndCorrespondences b f sourceI
  presentationEqualityTest
  commonViewTest
  updateCounterexample b
  emptyFamilyTest b
  let identityComponents = M.map (M.fromSet id) (sets (instanceData sourceI))
  revised <- ok (instanceOf (instanceSchema sourceI) (sourceData True))
  check "ID-preserving interpretation correction is not a natural transformation" (isLeft (natural sourceI revised identityComponents))
  deleted <- ok (edit [Delete "Clue" "r1",Delete "Clue" "r2"] sourceI)
  check "deletion has no total old-to-new map" (isLeft (natural sourceI deleted identityComponents))
  cNew <- ok (recordInstance b "base1" "source" (sourceData True) c >>= recordRevisionNote "base1" (RevisionNote (Just "base0") "changed interpretation" ["experience:correction"]) >>= adoptSystem "perception" "base1")
  check "active view becomes stale after a head change" (viewFreshness b "current" cNew == Right (Stale "base0" "base1"))
  check "pinned old snapshot remains explicit" (viewFreshness b "pinned" cNew == Right PinnedSnapshot)
  check "adopting stale output as active fails" (isLeft (adoptView b "wrong" (Active "perception") "group" cNew))
  cRevised <- ok (derive b "revised-join" "revised-joined" (JoinAlong "include") "base1" cNew)
  revisedD <- ok (lookupDerivation "revised-join" cRevised)
  let r1Choices = [rowElement (memberRow m) | (row,PiFamily ms) <- M.toList (derivationWitnesses revisedD),rowObject row == "Query",any ((==RowRef "base1" "Clue" "r1") . memberRow) ms,m <- ms,memberObject m == "Action"]
  check "structural correction changes the next action candidate with all payload text fixed" (r1Choices == ["a2"])
  check "temporal predecessors cannot form a cycle" (isLeft (recordRevisionNote "base0" (RevisionNote (Just "base1") "cycle" []) cNew))
  cHead <- ok (adoptSystem "derived-source" "rooted" cNew >>= adoptView b "derived-view" (Active "derived-source") "projection")
  check "an intermediate derived system can itself be the active source" (viewFreshness b "derived-view" cHead == Right Current)
  cHead' <- ok (adoptSystem "derived-source" "joined" cHead)
  check "source capture distinguishes intermediate revisions with the same ultimate ancestor" (viewFreshness b "derived-view" cHead' == Right (Stale "rooted" "joined"))
  cSigma <- ok (derive b "push-inclusion" "free-query" (PushAlong "include") "base0" cNew)
  lan <- ok (sigma f sourceI)
  eta <- ok (sigmaUnit lan)
  cWithCorrespondence <- ok (recordCorrespondence b "unit-correspondence" "base0" "free-query" "include" (components eta) cSigma)
  encoded <- ok (encodeCatalog b cWithCorrespondence) >>= E.evaluate . force
  decoded <- ok (decodeCatalog b encoded)
  check "normalized native catalog roundtrip including stale heads and nested witnesses" (decoded == cWithCorrespondence)
  check "definitions have native object and generator rows" (fmap S.size (elements encoded "catdb.catalog.object") == Right 10)
  check "no shared JSON Scalar is used in catalog" ("Scalar" `notElem` objects (definition (instanceSchema encoded)))
  forgedWitnessTest b encoded
  forgedLineageTest b encoded
  check "Pi result budget fails completely" (case piMigrationWithBudget (b {maxPiFamilies=1}) f sourceI of Left (BudgetExceeded PiFamilies _ 1) -> True; _ -> False)
  check "Pi search budget fails completely" (case piMigrationWithBudget (b {maxPiBindings=0}) f sourceI of Left (BudgetExceeded PiBindings 1 0) -> True; _ -> False)
  check "Pi family budget is shared across destination objects" (case piMigrationWithBudget (b {maxPiFamilies=2}) f sourceI of Left (BudgetExceeded PiFamilies 3 2) -> True; _ -> False)
  check "Sigma token budget is shared across destination objects" (case sigmaWithBudget (b {maxSigmaTokens=6}) f sourceI of Left (BudgetExceeded SigmaTokens 10 6) -> True; _ -> False)
  check "schema equation count is bounded" (case schemaWithBudget (b {maxSchemaEquations=0}) (queryDef True) of Left (BudgetExceeded SchemaEquations 1 0) -> True; _ -> False)
  check "schema congruence work is bounded before expansion" (case schemaWithBudget (b {maxEquationChecks=0}) (queryDef True) of Left (BudgetExceeded EquationChecks _ 0) -> True; _ -> False)
  check "instance equation evaluation work is bounded" (case instanceOfWithBudget (b {maxEquationChecks=0}) (instanceSchema joined) (instanceData joined) of Left (BudgetExceeded EquationChecks _ 0) -> True; _ -> False)
  check "schema path budget rejects exact expansion" (case schemaWithBudget (b {maxSchemaPaths=0}) sourceDef of Left (BudgetExceeded SchemaPaths _ 0) -> True; _ -> False)
  check "catalog decoding has a replay budget" (case decodeCatalog (b {maxCatalogReplays=0}) encoded of Left (NativeError (BudgetExceeded CatalogReplays _ 0)) -> True; _ -> False)
  check "native catalog materialization has a record budget" (case encodeCatalog (b {maxCatalogRecords=2}) c of Left (NativeError (BudgetExceeded CatalogRecords _ 2)) -> True; _ -> False)
  persistence b dir cWithCorrespondence encoded
  admissionTests b dir c
  putStrLn "PASS: native catalog, composed correspondences, adjunction triangles, revision counterexamples, 2/4 causal structure, typed provenance, budgets, atomic legacy composition"

equationsAndCorrespondences :: Budget -> Mapping -> Instance -> IO ()
equationsAndCorrespondences _ f i = do
  l <- ok (sigma f i)
  eta <- ok (sigmaUnit l)
  a <- ok (systemMorphism f i (sigmaInstance l) (components eta))
  e <- ok (schema (SchemaDef ["All"] [] []))
  g <- ok (mapping (codomain f) e (MappingDef (M.fromList [(o,"All") | o <- objects (definition (codomain f))]) (M.fromList [(arrow x,identity "All") | x <- arrows (definition (codomain f))])))
  lg <- ok (sigma g (sigmaInstance l))
  etaG <- ok (sigmaUnit lg)
  b <- ok (systemMorphism g (sigmaInstance l) (sigmaInstance lg) (components etaG))
  z <- ok (schema (SchemaDef ["All","Later"] [ArrowDef "later" "All" "Later"] []))
  h <- ok (mapping e z (MappingDef (M.singleton "All" "All") M.empty))
  lh <- ok (sigma h (sigmaInstance lg))
  etaH <- ok (sigmaUnit lh)
  c <- ok (systemMorphism h (sigmaInstance lg) (sigmaInstance lh) (components etaH))
  ab <- ok (composeSystemMorphism a b)
  bc <- ok (composeSystemMorphism b c)
  left' <- ok (composeSystemMorphism ab c)
  right' <- ok (composeSystemMorphism a bc)
  check "system correspondences associate across three schema changes" (left' == right')
  check "system correspondence left identity" (composeSystemMorphism (identitySystemMorphism i) a == Right a)
  check "system correspondence right identity" (composeSystemMorphism a (identitySystemMorphism (systemTarget a)) == Right a)
  sEta <- ok (sigmaNatural f eta)
  sEpsilon <- ok (sigmaCounit f (sigmaInstance l))
  check "Sigma-Delta triangle" (composeNatural sEta sEpsilon == Right (identityNatural (sigmaInstance l)))
  r <- ok (piMigration f i)
  pEta <- ok (piUnit f (piInstance r))
  epsilon <- ok (piCounit r)
  pEpsilon <- ok (piNatural f epsilon)
  check "Delta-Pi triangle" (composeNatural pEta pEpsilon == Right (identityNatural (piInstance r)))

updateCounterexample :: Budget -> IO ()
updateCounterexample b = do
  c <- ok (schema (SchemaDef ["L","Bridge","R"] [ArrowDef "left" "Bridge" "L",ArrowDef "right" "Bridge" "R"] []))
  d <- ok (schema (SchemaDef ["Merged"] [] []))
  f <- ok (mapping c d (MappingDef (M.fromList [(o,"Merged") | o <- objects (definition c)]) (M.fromList [(arrow a,identity "Merged") | a <- arrows (definition c)])))
  old <- ok (edit [Insert "L" "l" M.empty,Insert "R" "r" M.empty] (empty c))
  new <- ok (edit [Insert "Bridge" "b" (M.fromList [("left","l"),("right","r")])] old)
  let cs = M.map (M.fromSet id) (sets (instanceData old))
  u <- ok (natural old new cs)
  s0 <- ok (sigmaWithBudget b f old)
  s1 <- ok (sigmaWithBudget b f new)
  su <- ok (sigmaNatural f u)
  check "an inclusion can merge two previous Sigma classes" (fmap S.size (elements (sigmaInstance s0) "Merged") == Right 2 && fmap S.size (elements (sigmaInstance s1) "Merged") == Right 1 && maybe False ((==1) . S.size . S.fromList . M.elems) (M.lookup "Merged" (components su)))

emptyFamilyTest :: Budget -> IO ()
emptyFamilyTest b = do
  let def = SchemaDef [] [] []
  c0 <- ok (recordSchema b "empty" def emptyCatalog >>= recordSchema b "one" (SchemaDef ["Only"] [] []) >>= recordInstance b "nothing" "empty" (InstanceDef M.empty M.empty) >>= recordMapping b "empty-map" "empty" "one" (MappingDef M.empty M.empty) >>= derive b "empty-pi" "empty-family" (JoinAlong "empty-map") "nothing")
  d <- ok (lookupDerivation "empty-pi" c0)
  check "empty Pi family is explicit and carries no direct experience" (M.elems (derivationWitnesses d) == [PiFamily []])
  encoded <- ok (encodeCatalog b c0)
  check "empty family survives persistence" (decodeCatalog b encoded == Right c0)
  a <- ok (schema (SchemaDef ["A","B"] [] []))
  z <- ok (schema (SchemaDef ["One"] [] []))
  f <- ok (mapping a z (MappingDef (M.fromList [("A","One"),("B","One")]) M.empty))
  i <- ok (edit [Insert "A" "a" M.empty] (empty a))
  p <- ok (piMigration f i)
  check "Pi cannot always give a same-direction total system correspondence" (isLeft (systemMorphism f i (piInstance p) (M.fromList [("A",M.empty),("B",M.empty)])))

forgedWitnessTest :: Budget -> Instance -> IO ()
forgedWitnessTest b encoded = do
  values <- ok (rows encoded "catdb.catalog.witness" [("kind",Path "catdb.catalog.witness" ["catdb.catalog.witness.kind"])])
  case [r | (r,fields) <- values,M.lookup "kind" fields == Just "pi"] of
    row:_ -> do
      forged <- ok (edit [Update "catdb.catalog.witness" row (M.singleton "catdb.catalog.witness.kind" "pull")] encoded)
      check "outer-native-valid forged witness rejected on decode" (isLeft (decodeCatalog b forged))
    [] -> fail "fixture had no Pi witness"

forgedLineageTest :: Budget -> Instance -> IO ()
forgedLineageTest b encoded = do
  values <- ok (rows encoded "catdb.catalog.derivation" [("input",Path "catdb.catalog.derivation" ["catdb.catalog.derivation.input"]),("output",Path "catdb.catalog.derivation" ["catdb.catalog.derivation.output"])])
  case values of
    (row,fields):_ -> case M.lookup "output" fields of
      Just output -> do
        case edit [Update "catdb.catalog.derivation" row (M.singleton "catdb.catalog.derivation.input" output)] encoded of
          Left _ -> check "native dependent path rejects lineage tampering" True
          Right forged -> check "cyclic derivation rejected during replay" (isLeft (decodeCatalog b forged))
      Nothing -> fail "missing output"
    [] -> fail "fixture had no derivation"

persistence :: Budget -> FilePath -> Catalog -> Instance -> IO ()
persistence b dir c encoded = do
  legacySchema <- ok (schema (SchemaDef ["Original"] [] []))
  legacy <- ok (edit [Insert "Original" "owner-original-text" M.empty] (empty legacySchema))
  combined <- ok (combineInstances b legacy encoded)
  check "runtime projection preserves legacy rows exactly" (projectInstance b legacySchema combined == Right legacy)
  db <- createDatabase (dir </> "combined.catdb") combined >>= ok
  rejected <- transaction db (\_ -> Left (InternalInvariant "intentional rollback")) :: IO (Either StoreError ())
  check "failed validation transaction is observable" (isLeft rejected)
  check "failed transaction leaves the complete authority unchanged" . (==combined) =<< (snapshot db >>= ok)
  closeDatabase db >>= ok
  reopened <- openDatabaseWithBudget b (dir </> "combined.catdb") >>= ok
  snap <- snapshot reopened >>= ok
  check "single native store reopens the catalog and original payload together" (projectCatalog b snap == Right c && projectInstance b legacySchema snap == Right legacy)
  closeDatabase reopened >>= ok

presentationEqualityTest :: IO ()
presentationEqualityTest = do
  s <- ok (schema (SchemaDef ["X","Y"] [ArrowDef "a" "X" "Y",ArrowDef "b" "X" "Y"] [Equation (Path "X" ["a"]) (Path "X" ["b"])]))
  f <- ok (mapping s s (MappingDef (M.fromList [("X","X"),("Y","Y")]) (M.fromList [("a",Path "X" ["a"]),("b",Path "X" ["b"])])))
  check "mapping identity equality uses morphisms modulo equations" (composeMapping (identityMapping s) f == Right f && composeMapping f (identityMapping s) == Right f)
  i <- ok (edit [Insert "Y" "y" M.empty,Insert "X" "x" (M.fromList [("a","y"),("b","y")])] (empty s))
  let idS = identitySystemMorphism i
  check "system identity survives noncanonical path spellings" (composeSystemMorphism idS idS == Right idS)

commonViewTest :: IO ()
commonViewTest = do
  c <- ok (schema (SchemaDef ["X","Y"] [ArrowDef "edge" "X" "Y"] []))
  d <- ok (schema (SchemaDef ["P","Q"] [] []))
  a <- ok (schema (SchemaDef ["Shared"] [] []))
  check "the intended total assignment can fail to extend to a functor" (isLeft (mapping c d (MappingDef (M.fromList [("X","P"),("Y","Q")]) (M.singleton "edge" (identity "P")))))
  f <- ok (mapping a c (MappingDef (M.singleton "Shared" "X") M.empty))
  g <- ok (mapping a d (MappingDef (M.singleton "Shared" "P") M.empty))
  i <- ok (edit [Insert "Y" "y" M.empty,Insert "X" "x" (M.singleton "edge" "y")] (empty c))
  pulled <- ok (delta f i)
  pushed <- ok (sigma g pulled)
  check "a common-view span executes Sigma_G Delta_F without forcing the total assignment" (fmap S.size (elements (sigmaInstance pushed) "P") == Right 1 && elements (sigmaInstance pushed) "Q" == Right S.empty)

capacityCatalog :: Budget -> Int -> Either CatalogError Catalog
capacityCatalog b n = do
  let def = SchemaDef ["Item"] [] []
  c0 <- recordSchema b "capacity-schema" def emptyCatalog
  c1 <- recordInstance b "capacity-base" "capacity-schema" (InstanceDef (M.singleton "Item" S.empty) M.empty) c0
  foldM (\c index -> derive b (DerivationKey ("capacity-derivation-" <> T.pack (show index)))
      (InstanceRev ("capacity-output-" <> T.pack (show index))) (Generate M.empty) "capacity-base" c) c1 [1..n]

admissionTests :: Budget -> FilePath -> Catalog -> IO ()
admissionTests b dir piCatalog = do
  let noReplays = b {maxCatalogReplays=0}
      wide = b {maxCatalogReplays=257}
      replayFailure used allowed result = case result of
        Left (NativeError (BudgetExceeded CatalogReplays actual cap)) -> actual == used && cap == allowed
        _ -> False
  emptyHistory <- ok (capacityCatalog noReplays 0)
  emptyEncoded <- ok (encodeCatalog noReplays emptyHistory)
  check "a zero replay budget admits and reopens an empty history" (decodeCatalog noReplays emptyEncoded == Right emptyHistory)
  check "zero-budget record/derive/encode cannot save an unreopenable derivation" (replayFailure 1 0 (capacityCatalog noReplays 1))
  explicitEmpty <- ok (derive b "explicit-empty" "explicit-empty-output" (Generate (M.singleton "Item" S.empty)) "capacity-base" emptyHistory)
  explicitEncoded <- ok (encodeCatalog b explicitEmpty)
  check "validated empty root sets have a canonical exact roundtrip" (decodeCatalog b explicitEncoded == Right explicitEmpty)
  check "normalizing empty roots does not admit an unknown seed object" (isLeft (derive b "invalid-empty" "invalid-empty-output" (Generate (M.singleton "Unknown" S.empty)) "capacity-base" emptyHistory))
  oneHistory <- ok (capacityCatalog b 1)
  check "encode readmits an existing history against a lowered replay budget" (replayFailure 1 0 (encodeCatalog noReplays oneHistory))
  c256 <- ok (capacityCatalog b 256)
  encoded256 <- ok (encodeCatalog b c256)
  check "256 derivations roundtrip at the default boundary" (decodeCatalog b encoded256 == Right c256)
  check "the 257th derivation fails atomically under the default replay capacity" (replayFailure 257 256 (capacityCatalog b 257))
  c257 <- ok (capacityCatalog wide 257)
  check "encode cannot save a wider catalog under the narrower default budget" (replayFailure 257 256 (encodeCatalog b c257))
  encoded257 <- ok (encodeCatalog wide c257)
  check "an explicitly widened budget admits and reopens 257 derivations" (decodeCatalog wide encoded257 == Right c257)
  check "loading with an incompatible lower budget fails explicitly" (replayFailure 257 256 (decodeCatalog b encoded257))
  db <- createDatabase (dir </> "capacity-256.catdb") encoded256 >>= ok
  rejected <- transaction db (\_ -> case encodeCatalog b c257 of
    Left _ -> Left (InternalInvariant "catalog admission refused")
    Right replacement -> Right ((),replacement))
  check "failed catalog admission cannot replace the last reopenable file state" (isLeft rejected)
  closeDatabase db >>= ok
  reopened <- openDatabaseWithBudget b (dir </> "capacity-256.catdb") >>= ok
  saved <- snapshot reopened >>= ok
  check "the accepted 256-entry file reopens under the same budget" (decodeCatalog b saved == Right c256)
  closeDatabase reopened >>= ok
  let oneDef = SchemaDef ["Item"] [] []
  w0 <- ok (recordSchema b "witness-schema" oneDef emptyCatalog >>= recordInstance b "witness-base" "witness-schema" (InstanceDef (M.singleton "Item" (S.singleton "x")) M.empty))
  w1 <- ok (derive b "witness-1" "witness-out-1" (Generate (M.singleton "Item" (S.singleton "x"))) "witness-base" w0)
  w2 <- ok (derive b "witness-2" "witness-out-2" (Generate (M.singleton "Item" (S.singleton "x"))) "witness-base" w1)
  let lowWitness = b {maxWitnessNodes=2}
  check "individually small witnesses cannot exceed the aggregate save budget" (case encodeCatalog lowWitness w2 of Left (NativeError (BudgetExceeded WitnessNodes 4 2)) -> True; _ -> False)
  check "aggregate witness capacity is also checked by the smart derive path" (case derive lowWitness "witness-2" "witness-out-2" (Generate (M.singleton "Item" (S.singleton "x"))) "witness-base" w1 of Left (NativeError (BudgetExceeded WitnessNodes 4 2)) -> True; _ -> False)
  f <- ok (lookupMapping "include" piCatalog)
  i <- ok (lookupInstance "base0" piCatalog)
  (_,stats) <- ok (piMigrationWithStatistics b f i)
  let actualBindings = piExploredBindings stats
      fitting = b {maxPiBindings=actualBindings}
      tooSmall = b {maxPiBindings=actualBindings-1}
  check "Pi admission uses search work rather than only the result count" (actualBindings > piResultFamilies stats)
  fittingEncoded <- ok (encodeCatalog fitting piCatalog)
  check "a lowered budget that fits actual Pi work still saves and reopens" (decodeCatalog fitting fittingEncoded == Right piCatalog)
  check "an incompatible lowered search budget is refused before saving" (case encodeCatalog tooSmall piCatalog of Left (NativeError (BudgetExceeded PiBindings used allowed)) -> used == actualBindings && allowed == actualBindings-1; _ -> False)
  check "the private replay certificate is reconstructed when decoding native rows" ((decodeCatalog fitting fittingEncoded >>= encodeCatalog fitting) == Right fittingEncoded)
  innerSchemaAdmission b
  deltaCorrespondenceAdmission b
  putStrLn "PASS: replay admission 0/256/257, lowered-budget reopen, aggregate witness capacity, exact Pi search certificates, inner schema and correspondence footprints"

innerSchemaAdmission :: Budget -> IO ()
innerSchemaAdmission b = do
  let names = [Object ("A" <> T.pack (show n)) | n <- [0..7::Int]]
      as = [ArrowDef (Arrow (label <> T.pack (show n))) (Object ("A" <> T.pack (show n))) (Object ("A" <> T.pack (show (n+1)))) | n <- [0..6::Int],label <- ["a","b"]]
      eq = Equation (Path "A0" ["a0"]) (Path "A0" ["b0"])
      def = SchemaDef names as (replicate 50 eq)
      lower = b {maxEquationChecks=100000}
  c <- ok (recordSchema b "inner-equations" def emptyCatalog)
  check "the lowered equation budget can still encode the outer meta-schema" (not (isLeft (encodeCatalog lower emptyCatalog)))
  check "inner schema compilation work is checked before saving" (case encodeCatalog lower c of Left (NativeError (BudgetExceeded EquationChecks used 100000)) -> used > 100000; _ -> False)
  encoded <- ok (encodeCatalog b c)
  check "the admitted inner schema reopens under the same budget" (decodeCatalog b encoded == Right c)

deltaCorrespondenceAdmission :: Budget -> IO ()
deltaCorrespondenceAdmission b = do
  let os = [Object ("O" <> T.pack (show n)) | n <- [0..9::Int]]
      a = SchemaDef os [] []
      z = SchemaDef ["X"] [] []
      leftData = InstanceDef (M.fromList [(o,S.empty) | o <- os]) M.empty
      rightData = InstanceDef (M.singleton "X" (S.fromList [Element (T.pack (show n)) | n <- [1..100::Int]])) M.empty
  c0 <- ok (recordSchema b "delta-domain" a emptyCatalog >>= recordSchema b "delta-target" z)
  c1 <- ok (recordInstance b "delta-left" "delta-domain" leftData c0 >>= recordInstance b "delta-right" "delta-target" rightData)
  c2 <- ok (recordMapping b "delta-map" "delta-domain" "delta-target" (MappingDef (M.fromList [(o,"X") | o <- os]) M.empty) c1)
  c3 <- ok (recordCorrespondence b "delta-correspondence" "delta-left" "delta-right" "delta-map" (M.fromList [(o,M.empty) | o <- os]) c2)
  check "correspondence Delta materialization has its own cheap admission footprint" (case encodeCatalog (b {maxInstanceElements=500}) c3 of Left (NativeError (BudgetExceeded InstanceElements 1000 500)) -> True; _ -> False)
  encoded <- ok (encodeCatalog b c3)
  check "an admitted correspondence reopens under the same budget" (decodeCatalog b encoded == Right c3)
