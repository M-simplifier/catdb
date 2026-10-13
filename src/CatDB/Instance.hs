{-# LANGUAGE DeriveAnyClass #-}
module CatDB.Instance
  ( InstanceDef(..), Instance, instanceSchema, instanceData, instanceOf, empty
  , elements, evaluate, Edit(..), edit, select, generated, rows
  , Components, Natural, natural, naturalSource, naturalTarget, components
  , identityNatural, composeNatural, applyComponent
  ) where

import CatDB.Internal
import CatDB.Schema (endpoint)
import Control.Monad (foldM, forM, forM_)
import Data.Aeson (FromJSON, ToJSON)
import Data.Binary (Binary)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import GHC.Generics (Generic)

instanceOf :: Schema -> InstanceDef -> Either Error Instance
instanceOf s dat = do
  ensure InstanceShape (M.keysSet (sets dat) == objectSet s
                    && M.keysSet (functions dat) == M.keysSet (generators s))
  let i = Instance s dat
  forM_ (arrows (definition s)) $ \a -> do
    xs <- elements i (source a)
    ys <- elements i (target a)
    f <- lookupE (UnknownArrow (arrow a)) (arrow a) (functions dat)
    ensure (FunctionDomain (arrow a)) (M.keysSet f == xs)
    forM_ (M.toList f) $ \(x,y) ->
      ensure (ReferenceViolation (arrow a) x y) (y `S.member` ys)
  forM_ (equations (definition s)) $ \eq -> do
    xs <- elements i (start (left eq))
    forM_ (S.toList xs) $ \x -> do
      l <- evaluate i (left eq) x
      r <- evaluate i (right eq) x
      ensure (EquationViolation eq x) (l == r)
  pure i

empty :: Schema -> Instance
empty s = Instance s (InstanceDef (M.fromSet (const S.empty) (objectSet s))
                     (M.map (const M.empty) (generators s)))

evaluate :: Instance -> Path -> Element -> Either Error Element
evaluate i p x = do
  _ <- endpoint (instanceSchema i) p
  xs <- elements i (start p)
  ensure (MissingElement (start p) x) (x `S.member` xs)
  foldM step x (steps p)
  where
    step y a = do
      f <- lookupE (UnknownArrow a) a (functions (instanceData i))
      lookupE (InternalInvariant "non-total instance") y f

data Edit = Insert Object Element (Map Arrow Element)
          | Update Object Element (Map Arrow Element)
          | Delete Object Element
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON, Binary)

-- Validate once at the transaction boundary. This permits inserting referenced
-- rows in either order and changing both sides of an equation atomically.
edit :: [Edit] -> Instance -> Either Error Instance
edit ops i = foldM apply (instanceData i) ops >>= instanceOf s
  where
    s = instanceSchema i
    apply dat op = case op of
      Insert o x fields -> do
        xs <- lookupE (UnknownObject o) o (sets dat)
        ensure (DuplicateElement o x) (x `S.notMember` xs)
        let required = M.keysSet (M.filter ((==o) . source) (generators s))
        ensure (RowShape o x) (M.keysSet fields == required)
        setFields o x fields dat { sets = M.insert o (S.insert x xs) (sets dat) }
      Update o x fields -> do
        xs <- lookupE (UnknownObject o) o (sets dat)
        ensure (MissingElement o x) (x `S.member` xs)
        setFields o x fields dat
      Delete o x -> do
        xs <- lookupE (UnknownObject o) o (sets dat)
        ensure (MissingElement o x) (x `S.member` xs)
        let out = M.keysSet (M.filter ((==o) . source) (generators s))
        pure dat { sets = M.insert o (S.delete x xs) (sets dat)
                 , functions = M.mapWithKey (\a f -> if a `S.member` out then M.delete x f else f) (functions dat) }
    setFields o x fields dat = do
      forM_ (M.keys fields) $ \a -> do
        g <- lookupE (UnknownArrow a) a (generators s)
        ensure (RowShape o x) (source g == o)
      pure dat { functions = M.foldlWithKey' (\fs a y -> M.adjust (M.insert x y) a fs) (functions dat) fields }

-- Greatest subfunctor whose component at o is contained in the requested set.
-- Referencing rows that would become dangling are removed to a fixed point.
-- Unrelated objects are retained. This is a query, not a cascading mutation.
select :: Object -> Set Element -> Instance -> Either Error Instance
select o keep i = do
  xs <- elements i o
  ensure (UnknownObject o) (keep `S.isSubsetOf` xs)
  let original = instanceData i
      shrink ss = foldl remove ss (arrows (definition (instanceSchema i)))
      remove ss a = case (M.lookup (source a) ss, M.lookup (target a) ss, M.lookup (arrow a) (functions original)) of
        (Just from, Just to, Just f) -> M.insert (source a)
          (S.filter (\x -> maybe False (`S.member` to) (M.lookup x f)) from) ss
        _ -> ss
      fixpoint ss = let next = shrink ss in if next == ss then ss else fixpoint next
      kept = fixpoint (M.insert o keep (sets original))
      fs = M.mapWithKey (\a f -> case M.lookup a (generators (instanceSchema i)) of
              Just g -> M.restrictKeys f (M.findWithDefault S.empty (source g) kept)
              Nothing -> f) (functions original)
  instanceOf (instanceSchema i) (InstanceDef kept fs)

rows :: Instance -> Object -> [(Text,Path)] -> Either Error [(Element,Map Text Element)]
rows i o columns = do
  xs <- elements i o
  forM_ columns $ \(_,p) -> do
    _ <- endpoint (instanceSchema i) p
    ensure (InvalidPath p) (start p == o)
  forM (S.toAscList xs) $ \x -> do
    fields <- forM columns $ \(label,p) -> (,) label <$> evaluate i p x
    pure (x, M.fromList fields)

-- Least subfunctor containing the roots. Unlike 'select', this removes
-- unrelated objects and unused value atoms, so a structured export need not
-- retain private values from a shared scalar object.
generated :: Map Object (Set Element) -> Instance -> Either Error Instance
generated roots i = do
  forM_ (M.toList roots) $ \(o,xs) -> do
    actual <- elements i o
    case S.lookupMin (xs S.\\ actual) of
      Just x -> Left (MissingElement o x)
      Nothing -> Right ()
  let s = instanceSchema i
      dat = instanceData i
      seed = M.union roots (M.fromSet (const S.empty) (objectSet s))
      extend ss a = let xs = M.findWithDefault S.empty (source a) ss
                        fn = M.findWithDefault M.empty (arrow a) (functions dat)
                        image = S.fromList (M.elems (M.restrictKeys fn xs))
                    in M.adjust (`S.union` image) (target a) ss
      close ss = let next = foldl extend ss (arrows (definition s))
                 in if next == ss then ss else close next
      kept = close seed
      fs = M.fromList [(arrow a,M.restrictKeys (M.findWithDefault M.empty (arrow a) (functions dat))
                              (M.findWithDefault S.empty (source a) kept)) | a <- arrows (definition s)]
  instanceOf s (InstanceDef kept fs)

natural :: Instance -> Instance -> Components -> Either Error Natural
natural i j cs = do
  ensure SchemaMismatch (instanceSchema i == instanceSchema j)
  ensure NaturalShape (M.keysSet cs == objectSet (instanceSchema i))
  forM_ (objects (definition (instanceSchema i))) $ \o -> do
    xs <- elements i o
    ys <- elements j o
    f <- lookupE NaturalShape o cs
    ensure NaturalShape (M.keysSet f == xs && all (`S.member` ys) (M.elems f))
  let n = Natural i j cs
  forM_ (arrows (definition (instanceSchema i))) $ \a -> do
    xs <- elements i (source a)
    forM_ (S.toList xs) $ \x -> do
      l <- applyComponent n (source a) x >>= evaluate j (Path (source a) [arrow a])
      r <- evaluate i (Path (source a) [arrow a]) x >>= applyComponent n (target a)
      ensure (NaturalityViolation (arrow a) x) (l == r)
  pure n

applyComponent :: Natural -> Object -> Element -> Either Error Element
applyComponent n o x = lookupE NaturalShape o (components n) >>= lookupE (MissingElement o x) x

identityNatural :: Instance -> Natural
identityNatural i = Natural i i (M.map (M.fromSet id) (sets (instanceData i)))

composeNatural :: Natural -> Natural -> Either Error Natural
composeNatural f g = do
  ensure SchemaMismatch (naturalTarget f == naturalSource g)
  cs <- M.traverseWithKey (\o -> traverse (applyComponent g o)) (components f)
  natural (naturalSource f) (naturalTarget g) cs
