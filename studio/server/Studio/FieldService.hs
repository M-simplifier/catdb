{-# LANGUAGE OverloadedStrings #-}
module Studio.FieldService (seed, runStage, status) where

import qualified CatDB as C
import Control.Monad (foldM, forM, unless)
import Data.List (nub, sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)
import Studio.Wire

rawKey, normalizedKey, operationKey, candidateKey :: C.SchemaKey
rawKey="fs:raw"; normalizedKey="fs:normalized"; operationKey="fs:operations"; candidateKey="fs:candidates"
rawSystem :: C.SystemKey
rawSystem="現場サービス原本"
sigmaView, deltaView, piView :: C.ViewKey
sigmaView="1 顧客をそろえる"; deltaView="2 派遣の形で読む"; piView="3 担当できる組"

makeSchema :: [C.Object] -> [(C.Arrow,C.Object,C.Object)] -> [(C.Path,C.Path)] -> C.SchemaDef
makeSchema os as es = C.SchemaDef os [C.ArrowDef a s t | (a,s,t)<-as] [C.Equation a b | (a,b)<-es]
rawSchema :: C.SchemaDef
rawSchema = makeSchema ["営業顧客","請求顧客","サポート顧客","顧客キー","契約","依頼","資格","稼働枠","担当者","製品","訪問枠","地域"]
  [("crmKey","営業顧客","顧客キー"),("billingKey","請求顧客","顧客キー"),("supportKey","サポート顧客","顧客キー")
  ,("customerRegion","顧客キー","地域"),("contractCustomer","契約","請求顧客"),("contractProduct","契約","製品")
  ,("ticketCustomer","依頼","サポート顧客"),("ticketContract","依頼","契約"),("ticketProduct","依頼","製品"),("ticketSlot","依頼","訪問枠")
  ,("qualificationEngineer","資格","担当者"),("qualificationProduct","資格","製品")
  ,("availabilityEngineer","稼働枠","担当者"),("availabilitySlot","稼働枠","訪問枠"),("availabilityRegion","稼働枠","地域")]
  [(C.Path "依頼" ["ticketProduct"],C.Path "依頼" ["ticketContract","contractProduct"])
  ,(C.Path "依頼" ["ticketCustomer","supportKey"],C.Path "依頼" ["ticketContract","contractCustomer","billingKey"])]

aliases :: [C.Object]
aliases=["営業顧客","請求顧客","サポート顧客","顧客キー"]
crosswalks :: [C.Arrow]
crosswalks=["crmKey","billingKey","supportKey"]
normalizeObject :: C.Object -> C.Object
normalizeObject o=if o `elem` aliases then "顧客" else o
normalizePath :: C.Path -> C.Path
normalizePath (C.Path o as)=C.Path (normalizeObject o) (filter (`notElem` crosswalks) as)
normalizedSchema :: C.SchemaDef
normalizedSchema=makeSchema (nub (map normalizeObject (C.objects rawSchema)))
  [(C.arrow a,normalizeObject (C.source a),normalizeObject (C.target a)) | a<-C.arrows rawSchema,C.arrow a `notElem` crosswalks]
  [(normalizePath (C.left eq),normalizePath (C.right eq)) | eq<-C.equations rawSchema]

operationalArrows :: [(C.Arrow,C.Object,C.Object)]
operationalArrows=[("tp","依頼","製品"),("ts","依頼","訪問枠"),("tr","依頼","地域"),("qe","資格","担当者"),("qp","資格","製品")
  ,("ae","稼働枠","担当者"),("as","稼働枠","訪問枠"),("ar","稼働枠","地域")]
operationSchema, candidateSchema :: C.SchemaDef
operationSchema=makeSchema ["依頼","資格","稼働枠","担当者","製品","訪問枠","地域"] operationalArrows []
candidateSchema=makeSchema ("候補":C.objects operationSchema)
  (operationalArrows++[("ct","候補","依頼"),("cq","候補","資格"),("ca","候補","稼働枠")])
  [(C.Path "候補" ["ct","tp"],C.Path "候補" ["cq","qp"]),(C.Path "候補" ["ct","ts"],C.Path "候補" ["ca","as"])
  ,(C.Path "候補" ["ct","tr"],C.Path "候補" ["ca","ar"]),(C.Path "候補" ["cq","qe"],C.Path "候補" ["ca","ae"])]

customers :: [(C.Element,C.Element)]
customers=[("K01｜青葉メディカル","東"),("K02｜青葉メディカルサービス・別法人","東"),("K03｜港デザイン","西")
  ,("K04｜北辰フーズ","北"),("K05｜白波ラボ","西"),("K06｜森のケア","東")]
customer :: Int -> C.Element
customer n=maybe (C.Element ("不明な顧客"<>T.pack (show n))) fst (at n customers)
at :: Int -> [a] -> Maybe a
at n xs=case drop (n-1) xs of x:_ | n>0 -> Just x; _ -> Nothing
customerIds :: [C.Element]
customerIds=map fst customers
crm, billing, support :: Int -> C.Element
crm n=C.Element ("営業 C"<>T.pack (show n)<>"｜"<>C.elementName (customer n))
billing n=C.Element ("請求 B"<>T.pack (show n)<>"｜"<>C.elementName (customer n))
support n=C.Element ("保守 S"<>T.pack (show n)<>"｜"<>C.elementName (customer n))
contracts :: [(C.Element,Int,C.Element)]
contracts=[("契約01｜青葉・ポンプ",1,"ポンプ"),("契約02｜青葉・センサー",1,"センサー"),("契約03｜青葉サービス・冷却装置",2,"冷却装置")
  ,("契約04｜港・冷却装置",3,"冷却装置"),("契約05｜港・ポンプ",3,"ポンプ"),("契約06｜北辰・ポンプ",4,"ポンプ")
  ,("契約07｜北辰・センサー",4,"センサー"),("契約08｜白波・センサー",5,"センサー"),("契約09｜白波・冷却装置",5,"冷却装置")
  ,("契約10｜森のケア・ポンプ",6,"ポンプ"),("契約11｜森のケア・センサー",6,"センサー"),("契約12｜北辰・冷却装置",4,"冷却装置")]
tickets :: [(C.Element,Int,C.Element,C.Element,C.Element)]
tickets=[("依頼01｜青葉の定期点検",1,"契約01｜青葉・ポンプ","ポンプ","月曜午前")
  ,("依頼02｜青葉のセンサー交換",1,"契約02｜青葉・センサー","センサー","月曜午前")
  ,("依頼03｜別法人の冷却装置点検",2,"契約03｜青葉サービス・冷却装置","冷却装置","月曜午後")
  ,("依頼04｜港の冷却装置点検",3,"契約04｜港・冷却装置","冷却装置","月曜午前")
  ,("依頼05｜港のポンプ修理",3,"契約05｜港・ポンプ","ポンプ","火曜午前")
  ,("依頼06｜北辰のポンプ点検",4,"契約06｜北辰・ポンプ","ポンプ","月曜午前")
  ,("依頼07｜北辰のセンサー交換",4,"契約07｜北辰・センサー","センサー","火曜午前")
  ,("依頼08｜白波のセンサー点検",5,"契約08｜白波・センサー","センサー","月曜午後")
  ,("依頼09｜白波の冷却装置点検",5,"契約09｜白波・冷却装置","冷却装置","月曜午後")
  ,("依頼10｜森のケアのポンプ点検",6,"契約10｜森のケア・ポンプ","ポンプ","火曜午前")
  ,("依頼11｜森のケアのセンサー交換",6,"契約11｜森のケア・センサー","センサー","月曜午後")
  ,("依頼12｜北辰の冷却装置修理",4,"契約12｜北辰・冷却装置","冷却装置","月曜午後")]
qualifications :: [(C.Element,C.Element,C.Element)]
qualifications=[("資格01｜伊藤・ポンプ","伊藤","ポンプ"),("資格02｜伊藤・センサー","伊藤","センサー")
  ,("資格03｜佐藤・ポンプ","佐藤","ポンプ"),("資格04｜佐藤・冷却装置","佐藤","冷却装置")
  ,("資格05｜高橋・冷却装置","高橋","冷却装置"),("資格06｜高橋・センサー","高橋","センサー")
  ,("資格07｜田中・ポンプ","田中","ポンプ"),("資格08｜田中・冷却装置","田中","冷却装置")
  ,("資格09｜森・センサー","森","センサー"),("資格10｜森・冷却装置","森","冷却装置")
  ,("資格11｜小林・ポンプ","小林","ポンプ"),("資格12｜小林・センサー","小林","センサー")]
availability :: [(C.Element,C.Element,C.Element,C.Element)]
availability=[("稼働01｜伊藤・東・月曜午前","伊藤","東","月曜午前"),("稼働02｜伊藤・西・月曜午後","伊藤","西","月曜午後")
  ,("稼働03｜佐藤・東・月曜午前","佐藤","東","月曜午前"),("稼働04｜佐藤・東・月曜午後","佐藤","東","月曜午後")
  ,("稼働05｜高橋・西・月曜午前","高橋","西","月曜午前"),("稼働06｜高橋・北・火曜午前","高橋","北","火曜午前")
  ,("稼働07｜田中・北・月曜午前","田中","北","月曜午前"),("稼働08｜田中・西・火曜午前","田中","西","火曜午前")
  ,("稼働09｜森・西・月曜午後","森","西","月曜午後"),("稼働10｜森・北・火曜午前","森","北","火曜午前")
  ,("稼働11｜小林・東・火曜午前","小林","東","火曜午前"),("稼働12｜小林・北・月曜午後","小林","北","月曜午後")]

rawData :: C.InstanceDef
rawData=C.InstanceDef (M.fromList [(o,S.fromList xs) | (o,xs)<-tables]) (M.fromList [(a,M.fromList fs) | (a,fs)<-functions])
  where
    extraCRM="営業 C101｜青葉メディカル・本社"; extraSupport="保守 S101｜アオバ本社"
    tables=[("営業顧客",map crm [1..6]++[extraCRM]),("請求顧客",map billing [1..6]),("サポート顧客",map support [1..6]++[extraSupport])
      ,("顧客キー",customerIds),("契約",[x | (x,_,_)<-contracts]),("依頼",[x | (x,_,_,_,_)<-tickets])
      ,("資格",[x | (x,_,_)<-qualifications]),("稼働枠",[x | (x,_,_,_)<-availability])
      ,("担当者",["伊藤","佐藤","高橋","田中","森","小林"]),("製品",["ポンプ","冷却装置","センサー"])
      ,("訪問枠",["月曜午前","月曜午後","火曜午前"]),("地域",["東","西","北"])]
    functions=[("crmKey",[(crm n,customer n) | n<-[1..6]]++[(extraCRM,customer 1)])
      ,("billingKey",[(billing n,customer n) | n<-[1..6]])
      ,("supportKey",[(support n,customer n) | n<-[1..6]]++[(extraSupport,customer 1)])
      ,("customerRegion",customers),("contractCustomer",[(x,billing n) | (x,n,_)<-contracts]),("contractProduct",[(x,p) | (x,_,p)<-contracts])
      ,("ticketCustomer",[(x,if n==1 then extraSupport else support n) | (x,n,_,_,_)<-tickets])
      ,("ticketContract",[(x,k) | (x,_,k,_,_)<-tickets]),("ticketProduct",[(x,p) | (x,_,_,p,_)<-tickets]),("ticketSlot",[(x,s) | (x,_,_,_,s)<-tickets])
      ,("qualificationEngineer",[(x,e) | (x,e,_)<-qualifications]),("qualificationProduct",[(x,p) | (x,_,p)<-qualifications])
      ,("availabilityEngineer",[(x,e) | (x,e,_,_)<-availability]),("availabilityRegion",[(x,r) | (x,_,r,_)<-availability]),("availabilitySlot",[(x,s) | (x,_,_,s)<-availability])]

seed :: Either C.CatalogError C.Catalog
seed=do
  c<-foldM (\c (k,s)->C.recordSchema C.defaultBudget k s c) C.emptyCatalog
    [(rawKey,rawSchema),(normalizedKey,normalizedSchema),(operationKey,operationSchema),(candidateKey,candidateSchema)]
  c1<-C.recordInstance C.defaultBudget "field-source-1" rawKey rawData c >>= C.adoptSystem rawSystem "field-source-1"
    >>= C.recordRevisionNote "field-source-1" (C.RevisionNote Nothing "架空の現場サービス原本・明示的な顧客対応表" [])
  let f=C.MappingDef (M.fromList [(o,normalizeObject o) | o<-C.objects rawSchema])
        (M.fromList [(C.arrow a,if C.arrow a `elem` crosswalks then C.Path "顧客" [] else C.Path (normalizeObject (C.source a)) [C.arrow a]) | a<-C.arrows rawSchema])
      h=C.MappingDef (M.fromList [(o,o) | o<-C.objects operationSchema]) (M.fromList
        [("tp",C.Path "依頼" ["ticketProduct"]),("ts",C.Path "依頼" ["ticketSlot"]),("tr",C.Path "依頼" ["ticketCustomer","customerRegion"])
        ,("qe",C.Path "資格" ["qualificationEngineer"]),("qp",C.Path "資格" ["qualificationProduct"])
        ,("ae",C.Path "稼働枠" ["availabilityEngineer"]),("as",C.Path "稼働枠" ["availabilitySlot"]),("ar",C.Path "稼働枠" ["availabilityRegion"])])
      inclusion=C.MappingDef (M.fromList [(o,o) | o<-C.objects operationSchema])
        (M.fromList [(C.arrow a,C.Path (C.source a) [C.arrow a]) | a<-C.arrows operationSchema])
  C.recordMapping C.defaultBudget "fs:normalize" rawKey normalizedKey f c1
    >>= C.recordMapping C.defaultBudget "fs:dispatch-shape" operationKey normalizedKey h
    >>= C.recordMapping C.defaultBudget "fs:compatible" operationKey candidateKey inclusion

findValue :: Ord k => Text -> k -> M.Map k a -> Either C.CatalogError a
findValue message k=maybe (Left (C.CatalogInvalid message)) Right . M.lookup k
sourceHead :: C.Catalog -> Either C.CatalogError C.InstanceRev
sourceHead=findValue "現場サービスの原本がありません。" rawSystem . C.catalogHeads
stageOutput :: C.ViewKey -> C.Catalog -> Either C.CatalogError C.InstanceRev
stageOutput key c=do
  adoption<-findValue "先の段階を計算してください。" key (C.catalogViews c)
  freshness<-C.viewFreshness C.defaultBudget key c
  unless (freshness==C.Current) (Left (C.CatalogInvalid "元の原本が進んでいます。体験の道順から、三段を再計算してください。"))
  C.derivationOutput <$> C.lookupDerivation (C.adoptionDerivation adoption) c
runStage :: Text -> C.Catalog -> Either C.CatalogError (C.Catalog,Maybe C.InstanceRev)
runStage stage c=case stage of
  "all" -> do
    (a,_)<-runStage "sigma" c
    (b,_)<-runStage "delta" a
    runStage "pi" b
  "sigma" -> sourceHead c >>= \r -> run sigmaView (C.PushAlong "fs:normalize") r
  "delta" -> stageOutput sigmaView c >>= \r -> run deltaView (C.PullAlong "fs:dispatch-shape") r
  "pi" -> stageOutput deltaView c >>= \r -> run piView (C.JoinAlong "fs:compatible") r
  _ -> Left (C.CatalogInvalid "段階は sigma / delta / pi / all から選びます。")
  where
    run view spec input=do
      raw<-sourceHead c
      let n=T.pack (show (C.catalogRevision c+1)); out=C.InstanceRev ("field-r"<>n); dk=C.DerivationKey ("field-d"<>n)
      next<-C.derive C.defaultBudget dk out spec input c >>= C.adoptView C.defaultBudget view (C.Active rawSystem) dk
      -- All three views capture the actual root, so downstream snapshots also become stale.
      adoption<-findValue "ビューがありません。" view (C.catalogViews next)
      unless (C.adoptionSourceRevision adoption==raw) (Left (C.CatalogInvalid "原本の版が一致しません。"))
      pure (next,Just out)

roots :: C.Catalog -> C.RowRef -> [C.RowRef]
roots c row=S.toList (go S.empty row)
  where
    go visited r | r `S.member` visited=S.empty
                 | otherwise=case C.explainRow r c of
                     Left _ -> S.empty
                     Right e -> case C.rowDerivation e of
                       Nothing -> S.singleton r
                       Just (_,w) -> S.unions (map (go (S.insert r visited)) (C.witnessInputs w))
wireRef :: C.RowRef -> WRef
wireRef (C.RowRef r o x)=WRef (C.unInstanceRev r) (C.objectName o) (C.elementName x)
originalName :: C.Object -> [C.RowRef] -> Text
originalName object refs=case nub [C.elementName (C.rowElement r) | r<-refs,C.rowObject r==object] of
  [name]->name
  []->"—"
  names->T.intercalate " / " names
dispatchRows :: C.Catalog -> C.InstanceRev -> Either C.CatalogError [WDispatch]
dispatchRows c rev=do
  i<-C.lookupInstance rev c
  let xs=S.toList (M.findWithDefault S.empty "候補" (C.sets (C.instanceData i)))
  rows<-forM xs $ \x->do
    explained<-C.explainRow (C.RowRef rev "候補" x) c
    let row=C.RowRef rev "候補" x
        members=case C.rowDerivation explained of Just (_,C.PiFamily ms)->ms; _->[]
        ownRoots o=nub [r | m<-members,C.memberObject m==o,r<-roots c (C.memberRow m),C.rowObject r==o]
        name o=originalName o (ownRoots o)
        origins=nub (concatMap ownRoots ["依頼","資格","稼働枠","担当者","製品","訪問枠","地域"])
    projections<-forM [("ct","依頼"),("cq","資格"),("ca","稼働枠")] $ \(a,o)->do
      value<-either (Left . C.NativeError) Right (C.evaluate i (C.Path "候補" [a]) x)
      pure (C.arrowName a,wireRef (C.RowRef rev o value))
    -- Names come from actual typed witnesses, never from generated s/p label matching.
    pure (WDispatch (wireRef row) (name "依頼") (name "担当者") (name "資格") (name "稼働枠") (name "製品") (name "訪問枠") (name "地域") (map wireRef origins) projections)
  pure (sortOn (\r->(dispatchTicket r,dispatchEngineer r,dispatchQualification r,dispatchAvailability r)) rows)

status :: C.Catalog -> Maybe WCourse
status c=either (const Nothing) Just $ do
  raw<-sourceHead c
  i<-C.lookupInstance raw c
  let countSource=[(C.objectName o,S.size xs) | (o,xs)<-M.toList (C.sets (C.instanceData i))]
      normalizedCount=case M.lookup sigmaView (C.catalogViews c) >>= (\a->M.lookup (C.adoptionDerivation a) (C.catalogDerivations c)) of
        Nothing->[]
        Just d->case C.lookupInstance (C.derivationOutput d) c of Left _->[]; Right n->[("Σ:顧客",S.size (M.findWithDefault S.empty "顧客" (C.sets (C.instanceData n))))]
      counts=countSource++normalizedCount
      histories=reverse (sortOn (\(k,_)->maybe (0::Integer) id (readMaybe (T.unpack (T.drop 7 (C.unDerivationKey k)))) )
        [(k,d) | (k,d)<-M.toList (C.catalogDerivations c),C.derivationSpec d==C.JoinAlong "fs:compatible"])
  runs<-forM (take 3 histories) $ \(dk,d)->do
    source<-C.rootInput C.defaultBudget dk c
    rows<-dispatchRows c (C.derivationOutput d)
    pure (WCourseRun (C.unInstanceRev (C.derivationOutput d)) (C.unInstanceRev source) rows)
  let ticketIds=case runs of
        []->[]
        latest:_->case C.lookupInstance (C.InstanceRev (courseRunSource latest)) c of
          Left _->[]
          Right origin->map C.elementName (S.toList (M.findWithDefault S.empty "依頼" (C.sets (C.instanceData origin))))
      perTicket=case runs of
        []->[]
        latest:_->[(ticket,length (filter ((==ticket) . dispatchTicket) (courseRunRows latest))) | ticket<-ticketIds]
      near=nearMiss i
  pure (WCourse (C.unInstanceRev raw) counts runs perTicket near)

nearMiss :: C.Instance -> [WCondition]
nearMiss i=case sequence comparisons of
  Left _->[]
  Right values->[WCondition label (C.elementName a) (C.elementName b) (a==b) | (label,a,b)<-values]
  where
    value o path row=C.evaluate i (C.Path o path) row
    compareValues label a b=(,,) label <$> a <*> b
    ticket="依頼02｜青葉のセンサー交換"; qualification="資格03｜佐藤・ポンプ"; available="稼働03｜佐藤・東・月曜午前"
    comparisons=[compareValues "製品" (value "依頼" ["ticketProduct"] ticket) (value "資格" ["qualificationProduct"] qualification)
      ,compareValues "訪問枠" (value "依頼" ["ticketSlot"] ticket) (value "稼働枠" ["availabilitySlot"] available)
      ,compareValues "地域" (value "依頼" ["ticketCustomer","supportKey","customerRegion"] ticket) (value "稼働枠" ["availabilityRegion"] available)
      ,compareValues "担当者" (value "資格" ["qualificationEngineer"] qualification) (value "稼働枠" ["availabilityEngineer"] available)]
