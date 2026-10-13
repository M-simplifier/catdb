module CatDB.Migration
  ( delta, deltaNatural
  , Lan, sigma, sigmaInstance, sigmaOrigins, sigmaUnit, sigmaFactor, sigmaNatural
  , Ran, piMigration, piInstance, piFamilies, piCounit, piFactor, piNatural
  , Query, pull, push, joinOver, thenQ, runQuery, querySource, queryTarget
  ) where

import CatDB.Internal
import CatDB.Schema
import CatDB.Instance
import Control.Monad (forM)
import Control.Monad.State.Strict (StateT, evalStateT, get, put, lift)
import Data.List (minimumBy, sort)
import Data.Ord (comparing)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T

type Slot = (Object,Path)
type Token = (Object,Path,Element)
type Family = Map Slot Element

hom :: Schema -> Object -> Object -> [Path]
hom s a b = [p | p <- morphisms s, start p == a, M.lookup p (endpoints s) == Just b]

mapBy :: Ord k => [k] -> (k -> Either Error a) -> Either Error (Map k a)
mapBy ks f = M.fromList <$> mapM (\k -> (,) k <$> f k) ks

checkedInput :: Mapping -> Instance -> Either Error ()
checkedInput f i = ensure SchemaMismatch (instanceSchema i == domain f)

delta :: Mapping -> Instance -> Either Error Instance
delta f j = do
  ensure SchemaMismatch (instanceSchema j == codomain f)
  ss <- mapBy (objects (definition (domain f))) $ \c -> mapObject f c >>= elements j
  fs <- mapBy (arrows (definition (domain f))) $ \a -> do
    fc <- mapObject f (source a)
    xs <- elements j fc
    p <- mapPath f (Path (source a) [arrow a])
    mapBy (S.toAscList xs) (evaluate j p)
  instanceOf (domain f) (InstanceDef ss (M.mapKeys arrow fs))

deltaNatural :: Mapping -> Natural -> Either Error Natural
deltaNatural f n = do
  i <- delta f (naturalSource n)
  j <- delta f (naturalTarget n)
  cs <- mapBy (objects (definition (domain f))) $ \c -> do
    fc <- mapObject f c
    lookupE NaturalShape fc (components n)
  natural i j cs

data Lan = Lan
  { sigmaMapping :: Mapping, sigmaInput :: Instance, sigmaResult :: Instance
  , sigmaNames :: Map Object (Map Token Element)
  } deriving (Eq, Show)

sigmaInstance :: Lan -> Instance
sigmaInstance = sigmaResult

-- Pointwise left Kan extension: (Sigma_F I)(d) = colim_(F down d) I.
-- Tokens (c,p,x) denote x at c, transported along p : F c -> d.
sigma :: Mapping -> Instance -> Either Error Lan
sigma f i = do
  checkedInput f i
  names <- mapBy ds $ \d -> do
    tokens <- concat <$> forM cs (\c -> do
      fc <- mapObject f c
      xs <- elements i c
      pure [(c,p,x) | p <- hom dest fc d, x <- S.toAscList xs])
    ensure (QueryLimit "Sigma tokens per object" (toInteger (length tokens))) (length tokens <= 100000)
    relations <- concat <$> forM (arrows (definition (domain f))) (\a -> do
      fa <- mapPath f (Path (source a) [arrow a])
      ft <- mapObject f (target a)
      xs <- elements i (source a)
      concat <$> forM (hom dest ft d) (\q -> do
        p <- compose dest fa q
        forM (S.toAscList xs) (\x -> do
          y <- evaluate i (Path (source a) [arrow a]) x
          pure ((source a,p,x),(target a,q,y)))))
    let classes = quotient tokens relations
        reps = S.toAscList (S.fromList (M.elems classes))
        labels = M.fromList (zip reps (map (Element . ("s" <>) . T.pack . show) [(0::Int)..]))
    traverse (\r -> lookupE (InternalInvariant "Sigma representative") r labels) classes
  let ss = M.map (S.fromList . M.elems) names
  fs <- mapBy (arrows (definition dest)) $ \a -> do
    from <- lookupE MappingShape (source a) names
    to <- lookupE MappingShape (target a) names
    pairs <- forM (M.toList from) $ \((c,p,x),label) -> do
      q <- compose dest p (Path (source a) [arrow a])
      y <- lookupE (InternalInvariant "Sigma arrow") (c,q,x) to
      pure (label,y)
    pure (M.fromList pairs)
  result <- instanceOf dest (InstanceDef ss (M.mapKeys arrow fs))
  pure (Lan f i result names)
  where
    dest = codomain f
    ds = objects (definition dest)
    cs = objects (definition (domain f))

sigmaOrigins :: Lan -> Map Object (Map Element [Token])
sigmaOrigins = M.map (M.fromListWith (++) . map (\(t,x) -> (x,[t])) . M.toList) . sigmaNames

sigmaUnit :: Lan -> Either Error Natural
sigmaUnit l = do
  targetI <- delta f (sigmaInstance l)
  cs <- M.traverseWithKey (\c xs -> do
    fc <- mapObject f c
    names <- lookupE MappingShape fc (sigmaNames l)
    mapBy (S.toAscList xs) (\x -> lookupE NaturalShape (c,identity fc,x) names)) (sets (instanceData (sigmaInput l)))
  natural (sigmaInput l) targetI cs
  where f = sigmaMapping l

-- Given h : I -> Delta_F J, return the UNIQUE h# : Sigma_F I -> J.
-- h#([c,p,x]) = J(p)(h_c(x)). The quotient and naturality guarantee this is
-- independent of which representative is used.
sigmaFactor :: Lan -> Instance -> Natural -> Either Error Natural
sigmaFactor l j h = do
  dj <- delta (sigmaMapping l) j
  ensure SchemaMismatch (naturalSource h == sigmaInput l && naturalTarget h == dj)
  cs <- M.traverseWithKey (\_ names -> do
    pairs <- forM (M.toList names) $ \((c,p,x),label) -> do
      y <- applyComponent h c x >>= evaluate j p
      pure (label,y)
    pure (M.fromList pairs)) (sigmaNames l)
  natural (sigmaInstance l) j cs

sigmaNatural :: Mapping -> Natural -> Either Error Natural
sigmaNatural f h = do
  a <- sigma f (naturalSource h)
  b <- sigma f (naturalTarget h)
  u <- sigmaUnit b
  k <- composeNatural h u
  sigmaFactor a (sigmaInstance b) k

data Ran = Ran
  { piMapping :: Mapping, piInput :: Instance, piResult :: Instance
  , compatibleFamilies :: Map Object (Map Element Family)
  } deriving (Eq, Show)

piInstance :: Ran -> Instance
piInstance = piResult
piFamilies :: Ran -> Map Object (Map Element Family)
piFamilies = compatibleFamilies

-- Pointwise right Kan extension: (Pi_F I)(d) = lim_(d down F) I.
-- A row is a compatible family, not an unrestricted Cartesian product.
piMigration :: Mapping -> Instance -> Either Error Ran
piMigration f i = do
  checkedInput f i
  families <- mapBy ds $ \d -> do
    slots <- concat <$> forM cs (\c -> do
      fc <- mapObject f c
      pure [(c,p) | p <- hom dest d fc])
    domains <- forM slots $ \slot@(c,_) -> (,) slot . S.toAscList <$> elements i c
    constraints <- concat <$> forM (arrows (definition (domain f))) (\a -> do
      fc <- mapObject f (source a)
      fa <- mapPath f (Path (source a) [arrow a])
      forM (hom dest d fc) $ \p -> do
        q <- compose dest p fa
        pure ((source a,p), Path (source a) [arrow a], (target a,q)))
    arcs <- forM constraints $ \(u,a,v) -> do
      xs <- elements i (start a)
      fn <- mapBy (S.toAscList xs) (evaluate i a)
      pure (u,v,fn,M.fromListWith S.union [(y,S.singleton x) | (x,y) <- M.toList fn])
    valid <- sort <$> compatibleFamiliesSearch (M.fromList [(slot,S.fromList xs) | (slot,xs) <- domains]) arcs
    pure (M.fromList (zip (map (Element . ("p" <>) . T.pack . show) [(0::Int)..]) valid))
  let ss = M.map M.keysSet families
      reverseFamilies = M.map (M.fromList . map (\(x,fam) -> (fam,x)) . M.toList) families
  fs <- mapBy (arrows (definition dest)) $ \a -> do
    from <- lookupE NaturalShape (source a) families
    destSlots <- concat <$> forM cs (\c -> do
      fc <- mapObject f c
      pure [(c,p) | p <- hom dest (target a) fc])
    to <- lookupE NaturalShape (target a) reverseFamilies
    traverse (\family -> do
      restricted <- mapBy destSlots $ \(c,p) -> do
        q <- compose dest (Path (source a) [arrow a]) p
        lookupE NaturalShape (c,q) family
      lookupE (InternalInvariant "Pi restriction") restricted to) from
  result <- instanceOf dest (InstanceDef ss (M.mapKeys arrow fs))
  pure (Ran f i result families)
  where
    dest = codomain f
    ds = objects (definition dest)
    cs = objects (definition (domain f))

-- An indexed constraint join, with the same limit semantics as the definition.
-- Bind the smallest remaining domain, propagating both functions and inverse
-- images. Never materialize the Cartesian product merely to filter it later.
compatibleFamiliesSearch :: Map Slot (S.Set Element)
  -> [(Slot,Slot,Map Element Element,Map Element (S.Set Element))]
  -> Either Error [Family]
compatibleFamiliesSearch domains arcs = evalStateT (search M.empty domains) (0,0)
  where
    search :: Family -> Map Slot (S.Set Element) -> StateT (Int,Int) (Either Error) [Family]
    search bound remaining = do
      (nodes,results) <- get
      lift (ensure (QueryLimit "Pi explored bindings" (toInteger nodes)) (nodes < 1000000))
      put (nodes+1,results)
      if M.null remaining then do
        lift (ensure (QueryLimit "Pi result families" (toInteger results)) (results < 100000))
        put (nodes+1,results+1)
        pure [bound]
      else do
        let choices = [(slot,foldl (narrow bound slot) xs arcs) | (slot,xs) <- M.toList remaining]
        case choices of
          [] -> pure []
          _ -> do
            let (slot,xs) = minimumBy (comparing (\(key,values) -> (S.size values,key))) choices
            concat <$> mapM (\x -> search (M.insert slot x bound) (M.delete slot remaining)) (S.toAscList xs)
    narrow bound slot xs (u,v,fn,inverse)
      | u == v && slot == u = S.filter (\x -> M.lookup x fn == Just x) xs
      | slot == v, Just x <- M.lookup u bound = xs `S.intersection` maybe S.empty S.singleton (M.lookup x fn)
      | slot == u, Just y <- M.lookup v bound = xs `S.intersection` M.findWithDefault S.empty y inverse
      | otherwise = xs

piCounit :: Ran -> Either Error Natural
piCounit r = do
  from <- delta f (piInstance r)
  cs <- mapBy (objects (definition (domain f))) $ \c -> do
    fc <- mapObject f c
    families <- lookupE NaturalShape fc (piFamilies r)
    traverse (lookupE NaturalShape (c,identity fc)) families
  natural from (piInput r) cs
  where f = piMapping r

-- Given h : Delta_F J -> I, return the UNIQUE h# : J -> Pi_F I.
-- At y in J(d), its (c,p) component is h_c(J(p)(y)).
piFactor :: Ran -> Instance -> Natural -> Either Error Natural
piFactor r j h = do
  dj <- delta (piMapping r) j
  ensure SchemaMismatch (naturalSource h == dj && naturalTarget h == piInput r)
  cs <- M.traverseWithKey (\d families -> do
    xs <- elements j d
    let inverse = M.fromList [(fam,x) | (x,fam) <- M.toList families]
    slots <- concat <$> forM (objects (definition (domain (piMapping r)))) (\c -> do
      fc <- mapObject (piMapping r) c
      pure [(c,p) | p <- hom (instanceSchema j) d fc])
    mapBy (S.toAscList xs) $ \x -> do
      family <- mapBy slots $ \(c,p) -> evaluate j p x >>= applyComponent h c
      lookupE (InternalInvariant "Pi factor") family inverse) (piFamilies r)
  natural j (piInstance r) cs

piNatural :: Mapping -> Natural -> Either Error Natural
piNatural f h = do
  a <- piMigration f (naturalSource h)
  b <- piMigration f (naturalTarget h)
  e <- piCounit a
  k <- composeNatural e h
  piFactor b (piInstance a) k

data Program = Pull Mapping | Push Mapping | Join Mapping | Then Query Query
data Query = Query { queryInput :: Schema, queryOutput :: Schema, program :: Program }

querySource, queryTarget :: Query -> Schema
querySource = queryInput
queryTarget = queryOutput

pull, push, joinOver :: Mapping -> Query
pull f = Query (codomain f) (domain f) (Pull f)
push f = Query (domain f) (codomain f) (Push f)
joinOver f = Query (domain f) (codomain f) (Join f)

thenQ :: Query -> Query -> Either Error Query
thenQ q r = do
  ensure SchemaMismatch (queryTarget q == querySource r)
  pure (Query (querySource q) (queryTarget r) (Then q r))

runQuery :: Query -> Instance -> Either Error Instance
runQuery q i = do
  ensure SchemaMismatch (instanceSchema i == querySource q)
  case program q of
    Pull f -> delta f i
    Push f -> sigmaInstance <$> sigma f i
    Join f -> piInstance <$> piMigration f i
    Then a b -> runQuery a i >>= runQuery b
