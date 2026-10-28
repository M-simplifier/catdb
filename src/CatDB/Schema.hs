module CatDB.Schema
  ( Object(..), Arrow(..), Element(..), Path(..), ArrowDef(..), Equation(..)
  , SchemaDef(..), Schema, Error(..), schema, schemaWithBudget, definition, morphisms
  , endpoint, normalize, compose, equivalent, identity
  , MappingDef(..), Mapping, mapping, domain, codomain, mappingDef
  , mapObject, mapPath, identityMapping, composeMapping
  ) where

import CatDB.Budget
import CatDB.Internal
import Control.Monad (foldM, forM, forM_)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

identity :: Object -> Path
identity c = Path c []

-- Acyclic presentations give a finite, exact word problem. The size bound is
-- on the entire path language, before quotienting; rejection is explicit.
schema :: SchemaDef -> Either Error Schema
schema = schemaWithBudget defaultBudget

schemaWithBudget :: Budget -> SchemaDef -> Either Error Schema
schemaWithBudget budget def = do
  ensureBudget budget SchemaObjects (toInteger (length (objects def)))
  ensureBudget budget SchemaArrows (toInteger (length (arrows def)))
  ensureBudget budget SchemaEquations (toInteger (length (equations def)))
  unique DuplicateObject (objects def)
  unique DuplicateArrow (map arrow (arrows def))
  let os = S.fromList (objects def)
      gs = M.fromList [(arrow a,a) | a <- arrows def]
      outgoing o = filter ((==o) . source) (arrows def)
      paths seen o = do
        ensure (CyclicSchema o) (o `S.notMember` seen)
        children <- forM (outgoing o) $ \a -> do
          ps <- paths (S.insert o seen) (target a)
          pure [Path o (arrow a:steps p) | p <- ps]
        let ps = identity o : concat children
        ensureBudget budget SchemaPaths (toInteger (length (take (limit+1) ps)))
        pure ps
  forM_ (arrows def) $ \a -> do
    ensure (UnknownObject (source a)) (source a `S.member` os)
    ensure (UnknownObject (target a)) (target a `S.member` os)
  ps <- concat <$> mapM (paths S.empty) (objects def)
  ensureBudget budget SchemaPaths (toInteger (length ps))
  let end p = foldM (\o a -> do
                g <- lookupE (UnknownArrow a) a gs
                ensure (InvalidPath p) (source g == o)
                pure (target g)) (start p) (steps p)
  ends <- M.fromList <$> mapM (\p -> (,) p <$> end p) ps
  let check p = lookupE (InvalidPath p) p ends
  forM_ (equations def) $ \eq -> do
    l <- check (left eq)
    r <- check (right eq)
    ensure (NonParallel eq) (start (left eq) == start (right eq) && l == r)
  ensureBudget budget EquationChecks (schemaEquationChecks def ps)
  -- Every occurrence in every path, with arbitrary left and right context.
  -- Taking connected components generates precisely the congruence, since the
  -- entire free path category is finite and present here.
  let relation = [(p, Path (start p) (take k (steps p) ++ steps (right eq) ++ drop (k+n) (steps p)))
                 | p <- ps, eq <- equations def, let n = length (steps (left eq))
                 , k <- [0 .. length (steps p)-n]
                 , M.lookup (Path (start p) (take k (steps p))) ends == Just (start (left eq))
                 , take n (drop k (steps p)) == steps (left eq)]
      nf = quotient ps relation
  pure (Schema def gs nf ends (S.toAscList (S.fromList (M.elems nf))))
  where
    limit = fromInteger (max 0 (min (toInteger (maxBound :: Int) - 1) (maxSchemaPaths budget)))
    unique err xs = go S.empty xs
      where go _ [] = Right ()
            go seen (x:rest) = do
              ensure (err x) (x `S.notMember` seen)
              go (S.insert x seen) rest

endpoint :: Schema -> Path -> Either Error Object
endpoint s p = lookupE (InvalidPath p) p (endpoints s)

normalize :: Schema -> Path -> Either Error Path
normalize s p = lookupE (InvalidPath p) p (normalForms s)

compose :: Schema -> Path -> Path -> Either Error Path
compose s p q = do
  e <- endpoint s p
  _ <- endpoint s q
  ensure (InvalidPath q) (e == start q)
  normalize s (Path (start p) (steps p ++ steps q))

equivalent :: Schema -> Path -> Path -> Either Error Bool
equivalent s p q = (==) <$> normalize s p <*> normalize s q

mapping :: Schema -> Schema -> MappingDef -> Either Error Mapping
mapping c d def = do
  ensure MappingShape (M.keysSet (onObjects def) == objectSet c
                    && M.keysSet (onArrows def) == M.keysSet (generators c))
  forM_ (M.elems (onObjects def)) $ \o ->
    ensure (UnknownObject o) (o `S.member` objectSet d)
  let f = Mapping c d def
  forM_ (arrows (definition c)) $ \a -> do
    p <- lookupE MappingShape (arrow a) (onArrows def)
    from <- mapObject f (source a)
    to <- mapObject f (target a)
    actual <- endpoint d p
    ensure (InvalidPath p) (start p == from && actual == to)
  forM_ (equations (definition c)) $ \eq -> do
    l <- mapPath f (left eq)
    r <- mapPath f (right eq)
    ensure (MappingEquation eq) (l == r)
  pure f

mapObject :: Mapping -> Object -> Either Error Object
mapObject f o = lookupE (UnknownObject o) o (onObjects (mappingDef f))

mapPath :: Mapping -> Path -> Either Error Path
mapPath f p = do
  _ <- endpoint (domain f) p
  o <- mapObject f (start p)
  ps <- mapM (\a -> lookupE (UnknownArrow a) a (onArrows (mappingDef f))) (steps p)
  normalize (codomain f) (Path o (concatMap steps ps))

identityMapping :: Schema -> Mapping
identityMapping s = Mapping s s (MappingDef
  (M.fromSet id (objectSet s))
  (M.map (\a -> Path (source a) [arrow a]) (generators s)))

-- Traversal order: composeMapping f g is g after f.
composeMapping :: Mapping -> Mapping -> Either Error Mapping
composeMapping f g = do
  ensure SchemaMismatch (codomain f == domain g)
  os <- traverse (mapObject g) (onObjects (mappingDef f))
  as <- traverse (mapPath g) (onArrows (mappingDef f))
  mapping (domain f) (codomain g) (MappingDef os as)
