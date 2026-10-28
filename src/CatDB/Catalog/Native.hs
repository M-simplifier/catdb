module CatDB.Catalog.Native
  ( catalogSchemaDef, catalogSchema, encodeCatalog, decodeCatalog, projectCatalog
  , combineInstances, projectInstance ) where

import CatDB.Budget
import CatDB.Internal
import CatDB.Schema
import CatDB.Instance
import CatDB.Migration (deltaWithBudget)
import CatDB.Catalog.Core
import Control.Monad (foldM, forM, forM_)
import Control.Monad.State.Strict (StateT, execStateT, get, put, lift)
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR

prefix :: Text
prefix = "catdb.catalog."
obj :: Text -> Object
obj = Object . (prefix <>)
arr :: Text -> Text -> Arrow
arr table name = Arrow (prefix <> table <> "." <> name)

tables :: [(Text,[(Text,Text)])]
tables =
  [("label",[]),("ordinal",[])
  ,("meta",[("format","label"),("revision","ordinal")])
  ,("schema",[("key","label")])
  ,("object",[("owner","schema"),("name","label"),("index","ordinal")])
  ,("generator",[("owner","schema"),("name","label"),("source","object"),("target","object"),("index","ordinal")])
  ,("path",[("owner","schema"),("start","object")])
  ,("step",[("path","path"),("generator","generator"),("index","ordinal")])
  ,("equation",[("owner","schema"),("left","path"),("right","path"),("index","ordinal")])
  ,("instance",[("key","label"),("schema","schema")])
  ,("element",[("instance","instance"),("object","object"),("name","label")])
  ,("cell",[("instance","instance"),("generator","generator"),("source","element"),("target","element")])
  ,("mapping",[("key","label"),("domain","schema"),("codomain","schema")])
  ,("objectImage",[("mapping","mapping"),("source","object"),("target","object")])
  ,("arrowImage",[("mapping","mapping"),("source","generator"),("path","path")])
  ,("correspondence",[("key","label"),("source","instance"),("target","instance"),("mapping","mapping")])
  ,("component",[("correspondence","correspondence"),("source","element"),("target","element")])
  ,("derivation",[("key","label"),("input","instance"),("output","instance"),("kind","label")])
  ,("derivationMapping",[("derivation","derivation"),("mapping","mapping")])
  ,("seed",[("derivation","derivation"),("element","element")])
  ,("witness",[("derivation","derivation"),("result","element"),("kind","label")])
  ,("direct",[("witness","witness"),("input","element")])
  ,("sigmaToken",[("witness","witness"),("input","element"),("path","path"),("index","ordinal")])
  ,("piSlot",[("witness","witness"),("input","element"),("path","path"),("index","ordinal")])
  ,("anchor",[("name","label")])
  ,("binding",[("element","element"),("anchor","anchor")])
  ,("head",[("key","label"),("instance","instance")])
  ,("view",[("key","label"),("derivation","derivation"),("source","instance")])
  ,("viewActive",[("view","view"),("head","head")])
  ,("viewPinned",[("view","view"),("instance","instance")])
  ,("revisionNote",[("instance","instance"),("reason","label")])
  ,("predecessor",[("note","revisionNote"),("previous","instance")])
  ,("revisionAnchor",[("note","revisionNote"),("anchor","anchor"),("index","ordinal")])]

catalogSchemaDef :: SchemaDef
catalogSchemaDef = SchemaDef (map (obj . fst) tables)
  [ArrowDef (arr t f) (obj t) (obj destination) | (t,fields) <- tables,(f,destination) <- fields]
  (map (\(t,p,q) -> Equation (path t p) (path t q)) equalities)
  where
    path table [] = identity (obj table)
    path table (f:rest) = Path (obj table) (arr table f : follow table f rest)
    follow table f rest = case lookup table tables >>= lookup f of
      Nothing -> []
      Just destination -> steps (path destination rest)
    equalities =
      [("generator",["owner"],["source","owner"]),("generator",["owner"],["target","owner"])
      ,("path",["owner"],["start","owner"]),("step",["path","owner"],["generator","owner"])
      ,("equation",["owner"],["left","owner"]),("equation",["owner"],["right","owner"])
      ,("element",["instance","schema"],["object","owner"])
      ,("cell",["instance"],["source","instance"]),("cell",["instance"],["target","instance"])
      ,("cell",["generator","source"],["source","object"]),("cell",["generator","target"],["target","object"])
      ,("objectImage",["mapping","domain"],["source","owner"])
      ,("objectImage",["mapping","codomain"],["target","owner"])
      ,("arrowImage",["mapping","domain"],["source","owner"])
      ,("arrowImage",["mapping","codomain"],["path","owner"])
      ,("correspondence",["source","schema"],["mapping","domain"])
      ,("correspondence",["target","schema"],["mapping","codomain"])
      ,("component",["correspondence","source"],["source","instance"])
      ,("component",["correspondence","target"],["target","instance"])
      ,("witness",["derivation","output"],["result","instance"])
      ,("direct",["witness","derivation","input"],["input","instance"])
      ,("sigmaToken",["witness","derivation","input"],["input","instance"])
      ,("piSlot",["witness","derivation","input"],["input","instance"])]

catalogSchema :: Budget -> Either CatalogError Schema
catalogSchema b = native (schemaWithBudget b catalogSchemaDef)

key :: [Text] -> Element
key = Element . T.concat . map (\t -> T.pack (show (T.length t)) <> ":" <> t)
sid :: SchemaKey -> Element
sid k = key ["schema",unSchemaKey k]
oid :: SchemaKey -> Object -> Element
oid k o = key ["object",unSchemaKey k,objectName o]
gid :: SchemaKey -> Arrow -> Element
gid k a = key ["generator",unSchemaKey k,arrowName a]
iid :: InstanceRev -> Element
iid r = key ["instance",unInstanceRev r]
mid :: MappingKey -> Element
mid k = key ["mapping",unMappingKey k]
did :: DerivationKey -> Element
did k = key ["derivation",unDerivationKey k]
eid :: RowRef -> Element
eid (RowRef r o x) = key ["element",unInstanceRev r,objectName o,elementName x]
aid :: AnchorId -> Element
aid a = key ["anchor",unAnchorId a]
ordinal :: Integral a => a -> Element
ordinal = Element . T.pack . show . toInteger

type Writer = StateT (Integer,InstanceDef) (Either CatalogError)

write :: Budget -> Text -> Element -> [(Text,Element)] -> Writer ()
write b table row fields = do
  (count,dat) <- get
  columns <- lift (maybe (Left (CatalogInvalid "unknown native catalog table")) Right (lookup table tables))
  lift (invalid "native catalog row shape" (S.fromList (map fst fields) == S.fromList (map fst columns)))
  let currentRows = M.findWithDefault S.empty (obj table) (sets dat)
  lift (invalid "duplicate native catalog row" (row `S.notMember` currentRows))
  let atoms = [(obj destination,value) | (name,destination) <- columns,destination `elem` ["label","ordinal"],Just value <- [lookup name fields]]
      ss = foldl (\m (o,x) -> M.adjust (S.insert x) o m) (M.adjust (S.insert row) (obj table) (sets dat)) atoms
      added = sum [toInteger (S.size xs - S.size (M.findWithDefault S.empty o (sets dat))) | (o,xs) <- M.toList ss]
      nextCount = count+added
      fs = foldl (\m (name,value) -> M.adjust (M.insert row value) (arr table name) m) (functions dat) fields
  lift (native (ensureBudget b CatalogRecords nextCount))
  put (nextCount,InstanceDef ss fs)

writePath :: Budget -> Element -> SchemaKey -> Path -> Writer Element
writePath b context sk p = do
  let pid = key ["path",elementName context]
  write b "path" pid [("owner",sid sk),("start",oid sk (start p))]
  forM_ (zip [(0::Integer)..] (steps p)) $ \(n,a) ->
    write b "step" (key ["step",elementName pid,T.pack (show n)]) [("path",pid),("generator",gid sk a),("index",ordinal n)]
  pure pid

encodeCatalog :: Budget -> Catalog -> Either CatalogError Instance
encodeCatalog b c = do
  checkReplayBudget b c
  s <- catalogSchema b
  (_,dat) <- execStateT build (0,instanceData (empty s))
  native (instanceOfWithBudget b s dat)
  where
    emit = write b
    build = do
      emit "meta" (key ["meta"]) [("format","1"),("revision",ordinal (cRevision c))]
      forM_ (M.toList (cSchemas c)) $ \(sk,s) -> do
        emit "schema" (sid sk) [("key",Element (unSchemaKey sk))]
        forM_ (zip [(0::Integer)..] (objects (definition s))) $ \(n,o) ->
          emit "object" (oid sk o) [("owner",sid sk),("name",Element (objectName o)),("index",ordinal n)]
        forM_ (zip [(0::Integer)..] (arrows (definition s))) $ \(n,a) ->
          emit "generator" (gid sk (arrow a)) [("owner",sid sk),("name",Element (arrowName (arrow a))),("source",oid sk (source a)),("target",oid sk (target a)),("index",ordinal n)]
        forM_ (zip [(0::Integer)..] (equations (definition s))) $ \(n,eq) -> do
          let eqid = key ["equation",unSchemaKey sk,T.pack (show n)]
          l <- writePath b (key [elementName eqid,"left"]) sk (left eq)
          r <- writePath b (key [elementName eqid,"right"]) sk (right eq)
          emit "equation" eqid [("owner",sid sk),("left",l),("right",r),("index",ordinal n)]
      forM_ (M.toList (cInstances c)) $ \(rev,(sk,i)) -> do
        emit "instance" (iid rev) [("key",Element (unInstanceRev rev)),("schema",sid sk)]
        forM_ (M.toList (sets (instanceData i))) $ \(o,xs) -> forM_ (S.toList xs) $ \x ->
          emit "element" (eid (RowRef rev o x)) [("instance",iid rev),("object",oid sk o),("name",Element (elementName x))]
        forM_ (arrows (definition (instanceSchema i))) $ \a ->
          forM_ (M.toList (M.findWithDefault M.empty (arrow a) (functions (instanceData i)))) $ \(x,y) ->
            emit "cell" (key ["cell",unInstanceRev rev,arrowName (arrow a),elementName x])
              [("instance",iid rev),("generator",gid sk (arrow a)),("source",eid (RowRef rev (source a) x)),("target",eid (RowRef rev (target a) y))]
      forM_ (M.toList (cMappings c)) $ \(mk,(a,z,f)) -> do
        emit "mapping" (mid mk) [("key",Element (unMappingKey mk)),("domain",sid a),("codomain",sid z)]
        forM_ (M.toList (onObjects (mappingDef f))) $ \(from,to) ->
          emit "objectImage" (key ["objectImage",unMappingKey mk,objectName from]) [("mapping",mid mk),("source",oid a from),("target",oid z to)]
        forM_ (M.toList (onArrows (mappingDef f))) $ \(from,to) -> do
          let row = key ["arrowImage",unMappingKey mk,arrowName from]
          p <- writePath b row z to
          emit "arrowImage" row [("mapping",mid mk),("source",gid a from),("path",p)]
      forM_ (M.toList (cCorrespondences c)) $ \(ck,(from,to,mk,cs)) -> do
        let row = key ["correspondence",unCorrespondenceKey ck]
        emit "correspondence" row [("key",Element (unCorrespondenceKey ck)),("source",iid from),("target",iid to),("mapping",mid mk)]
        f <- lift (lookupMapping mk c)
        forM_ (M.toList cs) $ \(o,values) -> do
          fo <- lift (native (mapObject f o))
          forM_ (M.toList values) $ \(x,y) -> emit "component" (key ["component",elementName row,objectName o,elementName x])
            [("correspondence",row),("source",eid (RowRef from o x)),("target",eid (RowRef to fo y))]
      forM_ (M.toList (cDerivations c)) $ \(dk,d) -> do
        let kind = case derivationSpec d of PullAlong _ -> "pull"; PushAlong _ -> "sigma"; JoinAlong _ -> "pi"; Generate _ -> "generated"
        emit "derivation" (did dk) [("key",Element (unDerivationKey dk)),("input",iid (derivationInput d)),("output",iid (derivationOutput d)),("kind",kind)]
        case derivationSpec d of
          Generate seeds -> forM_ (M.toList seeds) $ \(o,xs) -> forM_ (S.toList xs) $ \x ->
            emit "seed" (key ["seed",unDerivationKey dk,objectName o,elementName x]) [("derivation",did dk),("element",eid (RowRef (derivationInput d) o x))]
          spec -> do
            let mk = case spec of PullAlong k -> k; PushAlong k -> k; JoinAlong k -> k
            emit "derivationMapping" (key ["derivationMapping",unDerivationKey dk]) [("derivation",did dk),("mapping",mid mk)]
        (outputSchema,_) <- lift (findKey "instance" (unInstanceRev (derivationOutput d)) (derivationOutput d) (cInstances c))
        forM_ (M.toList (derivationWitnesses d)) $ \(result,w) -> do
          let wid = key ["witness",unDerivationKey dk,elementName (eid result)]
              wkind = case w of PullRef _ -> "pull"; SigmaClass _ -> "sigma"; PiFamily _ -> "pi"; GeneratedRef _ -> "generated"
          emit "witness" wid [("derivation",did dk),("result",eid result),("kind",wkind)]
          case w of
            PullRef r -> direct wid r
            GeneratedRef r -> direct wid r
            SigmaClass ts -> forM_ (zip [(0::Integer)..] ts) $ \(n,Transport p r) -> slot "sigmaToken" outputSchema wid n p r
            PiFamily ms -> forM_ (zip [(0::Integer)..] ms) $ \(n,FamilyMember _ p r) -> slot "piSlot" outputSchema wid n p r
      let anchors = S.unions (M.elems (cBindings c)) `S.union` S.fromList (concatMap revisionAnchors (M.elems (cNotes c)))
      forM_ (S.toList anchors) $ \a -> emit "anchor" (aid a) [("name",Element (unAnchorId a))]
      forM_ (M.toList (cBindings c)) $ \(row,as) -> forM_ (S.toList as) $ \a ->
        emit "binding" (key ["binding",elementName (eid row),unAnchorId a]) [("element",eid row),("anchor",aid a)]
      forM_ (M.toList (cHeads c)) $ \(sk,rev) -> emit "head" (headId sk) [("key",Element (unSystemKey sk)),("instance",iid rev)]
      forM_ (M.toList (cViews c)) $ \(vk,ViewAdoption binding dk captured) -> do
        let vid = key ["view",unViewKey vk]
        emit "view" vid [("key",Element (unViewKey vk)),("derivation",did dk),("source",iid captured)]
        case binding of
          Active sk -> emit "viewActive" (key ["viewActive",unViewKey vk]) [("view",vid),("head",headId sk)]
          Pinned rev -> emit "viewPinned" (key ["viewPinned",unViewKey vk]) [("view",vid),("instance",iid rev)]
      forM_ (M.toList (cNotes c)) $ \(rev,note) -> do
        let nid = key ["revisionNote",unInstanceRev rev]
        emit "revisionNote" nid [("instance",iid rev),("reason",Element (revisionReason note))]
        forM_ (revisionPrevious note) $ \old -> emit "predecessor" (key ["predecessor",unInstanceRev rev]) [("note",nid),("previous",iid old)]
        forM_ (zip [(0::Integer)..] (revisionAnchors note)) $ \(n,a) -> emit "revisionAnchor" (key ["revisionAnchor",unInstanceRev rev,T.pack (show n)]) [("note",nid),("anchor",aid a),("index",ordinal n)]
    headId sk = key ["head",unSystemKey sk]
    direct wid r = emit "direct" (key ["direct",elementName wid]) [("witness",wid),("input",eid r)]
    slot table sk wid n p r = do
      let row = key [table,elementName wid,T.pack (show n)]
      pid <- writePath b row sk p
      emit table row [("witness",wid),("input",eid r),("path",pid),("index",ordinal n)]

type Fields = Map Text Element
type NativeRows = Map Text [(Element,Fields)]

field :: Text -> Fields -> Either CatalogError Element
field name = maybe (Left (CatalogInvalid ("missing catalog field " <> name))) Right . M.lookup name
textField :: Text -> Fields -> Either CatalogError Text
textField name fields = elementName <$> field name fields
number :: Element -> Either CatalogError Integer
number x = case TR.decimal (elementName x) of
  Right (n,rest) | T.null rest -> Right n
  _ -> Left (CatalogInvalid "invalid native catalog ordinal")
one :: Text -> [a] -> Either CatalogError a
one _ [x] = Right x
one what _ = Left (CatalogInvalid ("expected exactly one " <> what))
ordered :: [(Element,Fields)] -> Either CatalogError [(Element,Fields)]
ordered rs = do
  tagged <- forM rs $ \r@(_,fs) -> do n <- field "index" fs >>= number; pure (n,r)
  let sorted = sortOn fst tagged
  invalid "non-contiguous catalog ordering" (map fst sorted == take (length sorted) [0..])
  pure (map snd sorted)

decodeCatalog :: Budget -> Instance -> Either CatalogError Catalog
decodeCatalog b input = do
  expected <- catalogSchema b
  invalid "not the catalog meta-schema; project a combined instance first" (instanceSchema input == expected)
  _ <- native (instanceOfWithBudget b expected (instanceData input))
  native (ensureBudget b CatalogRecords (sum (map (toInteger . S.size) (M.elems (sets (instanceData input))))))
  allRows <- M.fromList <$> forM tables (\(t,cols) -> do
    rs <- native (rows input (obj t) [(f,Path (obj t) [arr t f]) | (f,_) <- cols])
    pure (t,rs))
  decodeRows b input allRows

decodeRows :: Budget -> Instance -> NativeRows -> Either CatalogError Catalog
decodeRows b input allRows = do
  (_,meta) <- one "catalog metadata row" (rs "meta")
  format <- textField "format" meta
  invalid "unsupported catalog format" (format == "1")
  revision <- field "revision" meta >>= number
  c0 <- foldM readSchema emptyCatalog (rs "schema")
  c1 <- foldM readInstance c0 (rs "instance")
  c2 <- foldM readMapping c1 (rs "mapping")
  c3 <- foldM readCorrespondence c2 (rs "correspondence")
  ds <- forM (rs "derivation") $ \(row,fs) -> do
    dk <- DerivationKey <$> textField "key" fs
    from <- field "input" fs >>= instanceKey
    to <- field "output" fs >>= instanceKey
    kind <- textField "kind" fs
    spec <- if kind == "generated" then do
        seeds <- forM (owned "seed" "derivation" row) $ \(_,fields') -> field "element" fields' >>= rowRef
        pure (Generate (M.fromListWith S.union [(rowObject r,S.singleton (rowElement r)) | r <- seeds]))
      else do
        (_,mappingFields) <- one "derivation mapping" (owned "derivationMapping" "derivation" row)
        mk <- field "mapping" mappingFields >>= mappingKey
        case kind of "pull" -> pure (PullAlong mk); "sigma" -> pure (PushAlong mk); "pi" -> pure (JoinAlong mk)
                     _ -> Left (CatalogInvalid "unknown derivation kind")
    ws <- forM (owned "witness" "derivation" row) $ \(wid,wfs) -> do
      result <- field "result" wfs >>= rowRef
      wkind <- textField "kind" wfs
      w <- case wkind of
        "pull" -> PullRef <$> readDirect wid
        "generated" -> GeneratedRef <$> readDirect wid
        "sigma" -> SigmaClass <$> (ordered (owned "sigmaToken" "witness" wid) >>= traverse (readTransport . snd))
        "pi" -> PiFamily <$> (ordered (owned "piSlot" "witness" wid) >>= traverse (\(_,fields') -> do
          Transport p r <- readTransport fields'; pure (FamilyMember (rowObject r) p r)))
        _ -> Left (CatalogInvalid "unknown witness kind")
      pure (result,w)
    invalid "duplicate result witness" (length ws == M.size (M.fromList ws))
    pure (dk,Derivation from to spec (M.fromList ws))
  invalid "duplicate derivation key" (length ds == M.size (M.fromList ds))
  let c4 = c3 {cDerivations=M.fromList ds}
  c5 <- foldM (\c (_,fs) -> do r <- field "element" fs >>= rowRef; a <- field "anchor" fs >>= anchorKey; bindAnchor r a c) c4 (rs "binding")
  c6 <- foldM (\c (_,fs) -> do k <- SystemKey <$> textField "key" fs; rev <- field "instance" fs >>= instanceKey; adoptSystem k rev c) c5 (rs "head")
  views <- forM (rs "view") $ \(vid,fs) -> do
    vk <- ViewKey <$> textField "key" fs
    dk <- field "derivation" fs >>= derivationKey
    captured <- field "source" fs >>= instanceKey
    binding <- case (owned "viewActive" "view" vid,owned "viewPinned" "view" vid) of
      ([(_,a)],[]) -> Active <$> (field "head" a >>= headKey)
      ([],[(_,p)]) -> Pinned <$> (field "instance" p >>= instanceKey)
      _ -> Left (CatalogInvalid "a view must have exactly one source binding")
    pure (vk,ViewAdoption binding dk captured)
  invalid "duplicate view key" (length views == M.size (M.fromList views))
  c7 <- foldM readNote (c6 {cViews=M.fromList views}) (rs "revisionNote")
  result <- replayCatalog b (c7 {cRevision=revision})
  canonical <- encodeCatalog b result
  invalid "noncanonical, orphaned, or forged catalog records" (canonical == input)
  pure result
  where
    rs t = M.findWithDefault [] t allRows
    rowMaps = M.map M.fromList allRows
    readRow t k = findKey ("native " <> t) (elementName k) k (M.findWithDefault M.empty t rowMaps)
    indexes = M.fromList [((t,f),M.fromListWith (++) [(value,[row]) | row@(_,fs) <- rs t,Just value <- [M.lookup f fs]]) | (t,cols) <- tables,(f,_) <- cols]
    owned t f parent = M.findWithDefault [] parent (M.findWithDefault M.empty (t,f) indexes)
    schemaKey row = readRow "schema" row >>= fmap SchemaKey . textField "key"
    instanceKey row = readRow "instance" row >>= fmap InstanceRev . textField "key"
    mappingKey row = readRow "mapping" row >>= fmap MappingKey . textField "key"
    derivationKey row = readRow "derivation" row >>= fmap DerivationKey . textField "key"
    headKey row = readRow "head" row >>= fmap SystemKey . textField "key"
    anchorKey row = readRow "anchor" row >>= fmap AnchorId . textField "name"
    objectInfo row = do
      fs <- readRow "object" row
      sk <- field "owner" fs >>= schemaKey
      name <- Object <$> textField "name" fs
      pure (sk,name)
    generatorInfo row = do
      fs <- readRow "generator" row
      sk <- field "owner" fs >>= schemaKey
      name <- Arrow <$> textField "name" fs
      (a,from) <- field "source" fs >>= objectInfo
      (z,to) <- field "target" fs >>= objectInfo
      invalid "generator endpoint schema mismatch" (a == sk && z == sk)
      pure (sk,ArrowDef name from to)
    rowRef row = do
      fs <- readRow "element" row
      rev <- field "instance" fs >>= instanceKey
      (_,o) <- field "object" fs >>= objectInfo
      x <- Element <$> textField "name" fs
      pure (RowRef rev o x)
    readPath row = do
      fs <- readRow "path" row
      sk <- field "owner" fs >>= schemaKey
      (osk,o) <- field "start" fs >>= objectInfo
      invalid "path start schema mismatch" (sk == osk)
      ss <- ordered (owned "step" "path" row)
      as <- forM ss $ \(_,fields') -> do
        (gsk,a) <- field "generator" fields' >>= generatorInfo
        invalid "path generator schema mismatch" (sk == gsk)
        pure (arrow a)
      pure (sk,Path o as)
    readSchema c (row,fs) = do
      sk <- SchemaKey <$> textField "key" fs
      os <- ordered (owned "object" "owner" row) >>= traverse (fmap snd . objectInfo . fst)
      as <- ordered (owned "generator" "owner" row) >>= traverse (fmap snd . generatorInfo . fst)
      eqs <- ordered (owned "equation" "owner" row) >>= traverse (\(_,fields') -> do
        (a,l) <- field "left" fields' >>= readPath
        (z,r) <- field "right" fields' >>= readPath
        invalid "equation schema mismatch" (a == sk && z == sk)
        pure (Equation l r))
      recordSchema b sk (SchemaDef os as eqs) c
    readInstance c (row,fs) = do
      rev <- InstanceRev <$> textField "key" fs
      sk <- field "schema" fs >>= schemaKey
      s <- lookupSchema sk c
      es <- forM (owned "element" "instance" row) (rowRef . fst)
      let ss = M.unionWith S.union (M.fromListWith S.union [(rowObject r,S.singleton (rowElement r)) | r <- es]) (sets (instanceData (empty s)))
      cells <- forM (owned "cell" "instance" row) $ \(_,fields') -> do
        (_,a) <- field "generator" fields' >>= generatorInfo
        from <- field "source" fields' >>= rowRef
        to <- field "target" fields' >>= rowRef
        pure (arrow a,rowElement from,rowElement to)
      let fs' = M.unionWith M.union (M.fromListWith M.union [(a,M.singleton x y) | (a,x,y) <- cells]) (functions (instanceData (empty s)))
      invalid "duplicate native cell" (length cells == sum (map M.size (M.elems fs')))
      recordInstance b rev sk (InstanceDef ss fs') c
    readMapping c (row,fs) = do
      mk <- MappingKey <$> textField "key" fs
      a <- field "domain" fs >>= schemaKey
      z <- field "codomain" fs >>= schemaKey
      os <- forM (owned "objectImage" "mapping" row) $ \(_,fields') -> do
        (_,from) <- field "source" fields' >>= objectInfo
        (_,to) <- field "target" fields' >>= objectInfo
        pure (from,to)
      as <- forM (owned "arrowImage" "mapping" row) $ \(_,fields') -> do
        (_,from) <- field "source" fields' >>= generatorInfo
        (_,to) <- field "path" fields' >>= readPath
        pure (arrow from,to)
      invalid "duplicate mapping component" (length os == M.size (M.fromList os) && length as == M.size (M.fromList as))
      recordMapping b mk a z (MappingDef (M.fromList os) (M.fromList as)) c
    readCorrespondence c (row,fs) = do
      ck <- CorrespondenceKey <$> textField "key" fs
      from <- field "source" fs >>= instanceKey
      to <- field "target" fs >>= instanceKey
      mk <- field "mapping" fs >>= mappingKey
      i <- lookupInstance from c
      vals <- forM (owned "component" "correspondence" row) $ \(_,fields') -> do
        x <- field "source" fields' >>= rowRef
        y <- field "target" fields' >>= rowRef
        pure (rowObject x,rowElement x,rowElement y)
      let cs = M.unionWith M.union (M.fromListWith M.union [(o,M.singleton x y) | (o,x,y) <- vals]) (M.map (const M.empty) (sets (instanceData i)))
      invalid "duplicate natural component" (length vals == sum (map M.size (M.elems cs)))
      recordCorrespondence b ck from to mk cs c
    readDirect wid = do
      (_,fs) <- one "direct witness" (owned "direct" "witness" wid)
      field "input" fs >>= rowRef
    readTransport fs = do
      (_,p) <- field "path" fs >>= readPath
      r <- field "input" fs >>= rowRef
      pure (Transport p r)
    readNote c (row,fs) = do
      rev <- field "instance" fs >>= instanceKey
      reason <- textField "reason" fs
      previous <- case owned "predecessor" "note" row of
        [] -> pure Nothing
        [(_,p)] -> Just <$> (field "previous" p >>= instanceKey)
        _ -> Left (CatalogInvalid "multiple revision predecessors")
      anchors <- ordered (owned "revisionAnchor" "note" row) >>= traverse (\(_,a) -> field "anchor" a >>= anchorKey)
      recordRevisionNote rev (RevisionNote previous reason anchors) c

combineInstances :: Budget -> Instance -> Instance -> Either CatalogError Instance
combineInstances b a z = do
  let ad = definition (instanceSchema a); zd = definition (instanceSchema z)
  invalid "schema object collision while combining instances" (S.null (S.fromList (objects ad) `S.intersection` S.fromList (objects zd)))
  invalid "schema arrow collision while combining instances" (S.null (M.keysSet (functions (instanceData a)) `S.intersection` M.keysSet (functions (instanceData z))))
  s <- native (schemaWithBudget b (SchemaDef (objects ad ++ objects zd) (arrows ad ++ arrows zd) (equations ad ++ equations zd)))
  native (instanceOfWithBudget b s (InstanceDef (M.union (sets (instanceData a)) (sets (instanceData z))) (M.union (functions (instanceData a)) (functions (instanceData z)))))

projectInstance :: Budget -> Schema -> Instance -> Either CatalogError Instance
projectInstance b wanted combined = do
  let def = definition wanted
  f <- native (mapping wanted (instanceSchema combined) (MappingDef
    (M.fromList [(o,o) | o <- objects def])
    (M.fromList [(arrow a,Path (source a) [arrow a]) | a <- arrows def])))
  native (deltaWithBudget b f combined)

projectCatalog :: Budget -> Instance -> Either CatalogError Catalog
projectCatalog b combined = do
  s <- catalogSchema b
  projected <- projectInstance b s combined
  decodeCatalog b projected
