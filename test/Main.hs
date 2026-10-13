module Main (main) where

import CatDB
import Demo
import qualified Relay.Model as Relay
import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as LB
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (forM, forM_, unless, replicateM)
import Data.Either (isLeft, rights)
import qualified Data.ByteString as B
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, getFileSize)
import System.Environment (getArgs, getExecutablePath)
import System.Exit (die, ExitCode(..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import System.IO (hFlush, stdout)
import Test.QuickCheck (quickCheckWithResult, stdArgs, Args(..), chooseInt,
                       forAll, property, counterexample, isSuccess)
import Test.QuickCheck.Random (mkQCGen)

ok :: Show e => Either e a -> IO a
ok = either (die . show) pure

assert :: String -> Bool -> IO ()
assert label condition = unless condition (die ("FAIL: " ++ label))

-- Independent finite oracle: enumerate all component functions, then keep
-- exactly the natural ones. It does not call any migration construction.
naturals :: Instance -> Instance -> [Natural]
naturals i j = rights [natural i j cs | cs <- families]
  where
    choices o xs = [M.fromList (zip (S.toAscList xs) ys)
                  | ys <- replicateM (S.size xs) (S.toAscList (M.findWithDefault S.empty o (sets (instanceData j))))]
    families = foldr (\(o,xs) rest -> [M.insert o f r | f <- choices o xs, r <- rest])
                      [M.empty] (M.toAscList (sets (instanceData i)))

adjunction :: Mapping -> Instance -> Instance -> Either Error Bool
adjunction f i j = do
  l <- sigma f i
  r <- piMigration f i
  dj <- delta f j
  eta <- sigmaUnit l
  eps <- piCounit r
  let forwardS h = sigmaFactor l j h
      backwardS k = deltaNatural f k >>= composeNatural eta
      forwardP h = piFactor r j h
      backwardP k = deltaNatural f k >>= (\n -> composeNatural n eps)
      roundTrip a b hs = and <$> mapM (\h -> a h >>= b >>= pure . (==h)) hs
  s1 <- roundTrip forwardS backwardS (naturals i dj)
  s2 <- roundTrip backwardS forwardS (naturals (sigmaInstance l) j)
  p1 <- roundTrip forwardP backwardP (naturals dj i)
  p2 <- roundTrip backwardP forwardP (naturals j (piInstance r))
  identityS <- sigmaFactor l (sigmaInstance l) eta
  identityP <- piFactor r (piInstance r) eps
  pure (and [s1,s2,p1,p2,identityS == identityNatural (sigmaInstance l),identityP == identityNatural (piInstance r)])

walking :: Int -> Int -> Int -> Either Error (Mapping,Instance,Instance)
walking na nb nj = do
  c <- schema (SchemaDef ["A","B"] [] [])
  d <- schema (SchemaDef ["U","V"] [ArrowDef "uv" "U" "V"] [])
  f <- mapping c d (MappingDef (M.fromList [("A","U"),("B","V")]) M.empty)
  let es n = S.fromList [Element (T.pack (show k)) | k <- [1..n]]
  i <- instanceOf c (InstanceDef (M.fromList [("A",es na),("B",es nb)]) M.empty)
  j <- instanceOf d (InstanceDef (M.fromList [("U",es nj),("V",es nj)])
        (M.singleton "uv" (M.fromSet id (es nj))))
  pure (f,i,j)

pureTests :: IO ()
pureTests = do
  i <- ok todoInstance
  let s = instanceSchema i
      a = Path "Task" ["project","projectName"]
      b = Path "Task" ["owner","personProject","projectName"]
  assert "equations closed under composition" =<< ok (equivalent s a b)
  forM_ (morphisms s) $ \p -> do
    t <- ok (endpoint s p)
    assert "identity left" . (==p) =<< ok (compose s (identity (start p)) p)
    assert "identity right" . (==p) =<< ok (compose s p (identity t))
  forM_ [(p,q,r) | p <- morphisms s,q <- morphisms s,r <- morphisms s,
            endpoint s p == Right (start q),endpoint s q == Right (start r)] $ \(p,q,r) -> do
    x <- ok (compose s p q >>= \pq -> compose s pq r)
    y <- ok (compose s q r >>= compose s p)
    assert "associativity modulo equations" (x == y)
  assert "cycles explicitly rejected" (isLeft (schema (SchemaDef ["A"] [ArrowDef "loop" "A" "A"] [])))
  noEq <- ok (schema ((definition s) {equations=[]}))
  assert "map must preserve equations in schema, independent of data" (isLeft (mapping s noEq (mappingDef (identityMapping s))))
  assert "invalid equation update rejected" (isLeft (edit [Update "Task" "first" (M.singleton "project" "work")] i))
  assert "dangling reference rejected" (isLeft (edit [Delete "Person" "alice"] i))
  both <- ok (edit [Update "Task" "first" (M.fromList [("project","work"),("owner","bob")])] i)
  assert "atomic consistent update accepted" (evaluate both (Path "Task" ["owner"]) "first" == Right "bob")
  f <- ok (publicView s)
  view <- ok (delta f i)
  assert "composed public view" (evaluate view (Path "Card" ["assignee"]) "first" == Right "Alice")
  composed <- ok (composeMapping (identityMapping (domain f)) f)
  assert "Delta composition" (delta composed i == (delta f i >>= delta (identityMapping (domain f))))
  chain <- ok (thenQ (pull f) (pull (identityMapping (domain f))))
  assert "query chain" (runQuery chain i == Right view)
  removed <- ok (select "Project" (S.singleton "work") i)
  assert "selection returns closed subinstance" (elements removed "Task" == Right S.empty)
  (g,spanI) <- ok spanExample
  l <- ok (sigma g spanI)
  r <- ok (piMigration g spanI)
  assert "Sigma span gives two connected components" (fmap S.size (elements (sigmaInstance l) "Result") == Right 2)
  assert "Pi span gives three joined rows" (fmap S.size (elements (piInstance r) "Result") == Right 3)
  terminalJ <- ok (edit [Insert "Result" "x" M.empty,Insert "Result" "y" M.empty] (empty (codomain g)))
  assert "both adjunction bijections, all morphisms of span example" =<< ok (adjunction g spanI terminalJ)
  mappedS <- ok (sigmaNatural g (identityNatural spanI))
  mappedP <- ok (piNatural g (identityNatural spanI))
  assert "Sigma acts on identities" (mappedS == identityNatural (sigmaInstance l))
  assert "Pi acts on identities" (mappedP == identityNatural (piInstance r))
  -- Finite property family includes empty fibers and a nonidentity destination
  -- arrow; both Hom-set bijections are checked, not just expected row counts.
  result <- quickCheckWithResult stdArgs {maxSuccess=30, replay=Just (mkQCGen 20261013,0)} $
    forAll ((,,) <$> chooseInt (0,2) <*> chooseInt (0,2) <*> chooseInt (0,2)) $ \(na,nb,nj) ->
      case walking na nb nj >>= \(m,x,y) -> adjunction m x y of
        Right True -> property True
        other -> counterexample (show other) False
  assert "adjunction property family" (isSuccess result)
  -- Empty comma categories: colimit empty, limit singleton.
  c <- ok (schema (SchemaDef [] [] []))
  d <- ok (schema (SchemaDef ["Only"] [] []))
  e <- ok (mapping c d (MappingDef M.empty M.empty))
  el <- ok (sigma e (empty c))
  er <- ok (piMigration e (empty c))
  assert "initial/terminal empty diagram" (elements (sigmaInstance el) "Only" == Right S.empty &&
    fmap S.size (elements (piInstance er) "Only") == Right 1)
  putStrLn "PASS: category equations, functors, subinstances, queries, both adjunctions"

referenceTests :: IO ()
referenceTests = do
  initial <- ok Relay.demoWorkspace
  let command value input = case JSON.fromJSON value of
        JSON.Error e -> Left (InternalInvariant (T.pack e))
        JSON.Success parsed -> Relay.applyCommand parsed input
      cmd fields = JSON.object [(k,JSON.String v) | (k,v) <- fields]
      countPeople input = do
        value <- Relay.dashboard input
        pure (case value of
          JSON.Object root -> case KM.lookup "stats" root of
            Just (JSON.Object stats) -> KM.lookup "people" stats
            _ -> Nothing
          _ -> Nothing)
      link = cmd [("action","link"),("left","Team directory/2"),("right","Contractors/2")]
      assignment key = cmd [("action","assign"),("ticket","REQ-101"),("record",key)]
  assert "equal email identities coalesce" (countPeople initial == Right (Just (JSON.Number 5)))
  merged <- ok (command link initial)
  assert "explicit identity links coalesce transitively" (countPeople merged == Right (Just (JSON.Number 4)))
  linkIds <- ok (elements merged "Link")
  unlinked <- ok (edit [Delete "Link" key | key <- S.toList linkIds] merged)
  assert "unlink restores distinct identities" (countPeople unlinked == Right (Just (JSON.Number 5)))
  assert "cross-team assignment rejected by path equation" (case command (assignment "Team directory/2") initial of
    Left (EquationViolation _ _) -> True; _ -> False)
  assigned <- ok (command (assignment "Team directory/1") merged)
  closed <- ok (command (cmd [("action","status"),("ticket","REQ-103"),("status","closed")]) assigned)
  exported <- LB.toStrict . JSON.encode <$> ok (Relay.publicExport closed)
  assert "public view includes actual assignee" ("Maya Chen" `B.isInfixOf` exported)
  forM_ ["@example.test","Iris Park","Noah Quinn","Prepare the partner handoff","Team directory","Contractors"] $ \private ->
    assert ("public structured values exclude " ++ private) (not (LB.toStrict (LB.pack private) `B.isInfixOf` exported))
  -- The old Cartesian enumerator rejected this 160,000-choice diagram even
  -- though the limit has just 400 rows. Check the actual finite limit.
  s <- ok (schema (SchemaDef ["A","B"] [ArrowDef "f" "A" "B"] []))
  t <- ok (schema (SchemaDef ["R"] [] []))
  let xs = S.fromList [Element (T.pack (show n)) | n <- [1..400::Int]]
  i <- ok (instanceOf s (InstanceDef (M.fromList [("A",xs),("B",xs)]) (M.singleton "f" (M.fromSet id xs))))
  m <- ok (mapping s t (MappingDef (M.fromList [("A","R"),("B","R")]) (M.singleton "f" (identity "R"))))
  result <- ok (piMigration m i)
  assert "indexed Pi avoids Cartesian budget explosion" (fmap S.size (elements (piInstance result) "R") == Right 400)
  putStrLn "PASS: Relay identity links, assignment constraint, public value minimization, indexed Pi"

storeTests :: FilePath -> IO ()
storeTests dir = do
  createDirectoryIfMissing True dir
  let file = dir </> "local.catdb"
  initial <- ok todoInstance
  db <- createDatabase file initial >>= ok
  duplicate <- createDatabase file initial
  assert "create never overwrites" (isLeft duplicate)
  busy <- openDatabase file
  assert "same-process double open excluded" (case busy of Left Busy -> True; _ -> False)
  executable <- getExecutablePath
  (code,_,err) <- readProcessWithExitCode executable ["busy",file] ""
  assert ("other-process exclusive lock: " ++ err) (code == ExitSuccess)
  size <- getFileSize file
  rejected <- commit db [Update "Task" "first" (M.singleton "project" "work")]
  assert "failed transaction rejected" (isLeft rejected)
  assert "failed transaction writes nothing" . (==size) =<< getFileSize file
  -- Several application threads use the same connection, with no lost update.
  signals <- forM [1..8::Int] $ \n -> do
    signal <- newEmptyMVar
    _ <- forkIO $ commit db [Insert "Text" (Element (T.pack ("thread" ++ show n))) M.empty] >>= putMVar signal
    pure signal
  mapM_ (takeMVar >=> ok) signals
  commit db [Update "Task" "first" (M.singleton "status" "done")] >>= ok
  final <- snapshot db >>= ok
  assert "thread writes preserved" (fmap S.size (elements final "Text") == Right 13)
  copyDatabase db (dir </> "compact.catdb") >>= ok
  closeDatabase db >>= ok
  stale <- snapshot db
  assert "closed handle rejected" (stale == Left Closed)
  db2 <- openDatabase file >>= ok
  actual <- snapshot db2 >>= ok
  assert "reopen preserves committed data" (actual == final)
  closeDatabase db2 >>= ok
  copy <- openDatabase (dir </> "compact.catdb") >>= ok
  assert "compact backup equal" . (==final) =<< (snapshot copy >>= ok)
  closeDatabase copy >>= ok
  -- Observe a real second transaction frame; truncate at header/body/footer
  -- boundaries. This exercises recovery using the on-disk protocol itself.
  bytes <- B.readFile file
  let first = fromIntegral size
      tailFrame = B.drop first bytes
      frameLength = case B.unpack (B.take 8 (B.drop 8 tailFrame)) of
        octets -> sum [fromIntegral b * 256^k | (k,b) <- zip [0..7::Int] octets] + 40 :: Int
  forM_ [1,23,24,frameLength-1] $ \cut -> do
    let torn = dir </> ("torn" ++ show cut ++ ".catdb")
    B.writeFile torn (B.take (first+cut) bytes)
    recoveredDb <- openDatabase torn >>= ok
    n <- recoveredBytes recoveredDb >>= ok
    assert "torn frame recovery amount" (n == toInteger cut)
    restored <- snapshot recoveredDb >>= ok
    assert "torn commit never partially appears" (restored == initial)
    commit recoveredDb [Insert "Text" "after-recovery" M.empty] >>= ok
    closeDatabase recoveredDb >>= ok
  let damaged = dir </> "corrupt.catdb"
  B.writeFile damaged (B.take 25 bytes <> B.singleton 255 <> B.drop 26 bytes)
  corrupt <- openDatabase damaged
  assert "complete corrupt frame fails closed" (case corrupt of Left (Corrupt _ _) -> True; _ -> False)
  -- Actual process termination without close: acknowledged commit survives and
  -- the OS releases the lock. Parent CLI kills the child after its marker.
  putStrLn "PASS: durable reopen, validation rollback, threads, process lock, compact copy, torn tails, corruption"
  where (>=>) f g x = f x >>= g

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["busy",file] -> do
      result <- openDatabase file
      assert "busy in child" (case result of Left Busy -> True; _ -> False)
    ["hold",file] -> do
      i <- ok todoInstance
      db <- createDatabase file i >>= ok
      commit db [Update "Task" "first" (M.singleton "status" "done")] >>= ok
      putStrLn "COMMITTED"
      hFlush stdout
      _ <- getLine
      closeDatabase db >>= ok
    [dir] -> pureTests >> referenceTests >> storeTests dir
    _ -> die "laws TEST_DIRECTORY | busy FILE | hold FILE"
