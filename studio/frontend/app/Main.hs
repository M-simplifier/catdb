{-# LANGUAGE CPP, JavaScriptFFI #-}
module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever,unless,when,forM_)
import Control.Monad.State.Strict (get,modify)
import Data.List (find)
import Data.Maybe (fromMaybe,listToMaybe,isJust)
import Miso (App,Component(..),Effect,component,defaultEvents,io,io_,issue,startApp)
import qualified Miso.Fetch as Fetch
import qualified Miso.JSON as J
import Miso.String (ms)
import qualified Browser
import Model
import Studio.Wire
import View (viewModel)

main :: IO ()
main = startApp defaultEvents app
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

app :: App Model Action
app = (component initialModel updateModel viewModel)
  {mount=Just Initialize,subs=[\sink -> forever (threadDelay 3000000 >> sink Poll)]}

updateModel :: Action -> Effect parent props Model Action
updateModel action = do
  handle action
  m <- get
  io_ (Browser.markDirty (dirty m))

handle :: Action -> Effect parent props Model Action
handle = \case
  Initialize -> io (Initialized <$> Browser.sessionToken)
  Initialized value -> modify (\m -> m {token=value}) >> issue Poll
  Poll -> do
    m <- get
    unless (polling m || blank (token m)) $ do
      modify (\x -> x {polling=True})
      Fetch.getJSON "/api/state" (headers m) (GotState . Fetch.body)
        stateFailure
  GotState state -> do
    m <- get
    modify (\x -> x {snapshotState=Just state,polling=False,validated=if currentVersion m==stateVersion state then validated x else Nothing
      ,fileInput=if snapshot m==Nothing && isJust (stateCourse state) then fromMaybe (fileInput x) (statePath state) else fileInput x})
    when (snapshot m==Nothing && not (isPending m)) initializeSelection
  NetworkFailed msg -> modify (\m -> m {polling=False,feedback=Just msg})
  Navigate target -> modify (\m -> m {page=target,picker=Nothing,feedback=Nothing,failure=Nothing})
  Edit field value -> modify (editField field value)
  ToggleMath -> modify (\m -> m {math=not (math m)})
  ToggleGuideTools -> modify (\m -> m {guideTools=not (guideTools m)})
  GuideOpenSource object key -> do
    m<-get
    mapM_ (\course->send (GuideInspecting object key) ["tag" J..= ("inspect"::S),"source" J..= courseSource course]) (fieldCourse m)
  GuideMakeDraft kind -> do
    m<-get
    mapM_ (\course->send (GuideDrafting kind) ["tag" J..= ("inspect"::S),"source" J..= courseSource course]) (fieldCourse m)
  GuideRepair -> do
    m<-get
    let proposedProduct=fieldTarget (workingData m) "依頼" "依頼01｜青葉の定期点検" "ticketProduct"
        contract=if proposedProduct==Just "センサー" then "契約02｜青葉・センサー" else "契約01｜青葉・ポンプ"
    modify (changeData (setTarget "依頼" "依頼01｜青葉の定期点検" "ticketContract" contract))
  FieldStage stage -> send (FieldComputing stage) ["tag" J..= ("fieldStage"::S),"stage" J..= stage]
  Dismiss -> modify (\m -> m {feedback=Nothing,failure=Nothing})
  Noop -> pure ()
  Guard next -> do
    m <- get
    if dirty m then modify (\x -> x {confirm=Just next}) else issue next
  ConfirmDiscard -> do
    m <- get
    modify (\x -> x {confirm=Nothing,draft=Nothing,validated=Nothing})
    mapM_ issue (confirm m)
  KeepDraft -> modify (\m -> m {confirm=Nothing})
  OpenRevision key focus view -> send (Inspecting key focus view) ["tag" J..= ("inspect" :: S),"source" J..= key]
  FocusObject object -> do
    modify (\m -> m {page=Records,focusedObject=object,focusedRow=recordId <$> listToMaybe (records (visibleData m) object),focusPath=[],picker=Nothing,explanation=Nothing,newRecordInput="",insertTargets=[]})
    explainFocused
  FocusRow object key -> do
    modify (\m -> m {focusedObject=object,focusedRow=Just key,focusPath=[],picker=Nothing,explanation=Nothing})
    explainFocused
  Pick object key arrow -> modify (\m -> m {focusedObject=object,focusedRow=Just key,picker=Just (object,key,arrow),focusPath=[]})
  ClosePicker -> modify (\m -> m {picker=Nothing})
  ChooseTarget target -> do
    m <- get
    case picker m of
      Nothing -> pure ()
      Just (o,x,a) -> modify ((\n -> n {picker=Nothing}) . changeData (setTarget o x a target))
  SetInsertTarget arrow target -> modify (\m -> m {insertTargets=replace arrow target (insertTargets m)})
  AddRecord -> do
    m <- get
    let key=trim (newRecordInput m); o=focusedObject m
    if blank key then localFailure "新しい行のIDを入力してください。"
      else if record (workingData m) o key/=Nothing then localFailure "その行IDは既にあります。"
      else do
        let fs=[(arrowId a,fromMaybe "" (lookup (arrowId a) (insertTargets m))) | a <- outgoing (currentSchema m) o]
        modify ((\n -> n {newRecordInput="",insertTargets=[],focusedRow=Just key}) . changeData (map (\t -> if tableObject t==o then t {tableRows=tableRows t++[WRow key fs]} else t)))
  DeleteRecord -> do
    m <- get
    case focusedRow m of
      Nothing -> pure ()
      Just key -> modify ((\n -> n {focusedRow=Nothing,picker=Nothing,explanation=Nothing}) . changeData
        (map (\t -> if tableObject t==focusedObject m then t {tableRows=filter ((/=key) . recordId) (tableRows t)} else t)))
  ShowCommitted value -> modify (\m -> m {committedVisible=value,picker=Nothing})
  DiscardDraft -> modify (\m -> m {draft=Nothing,validated=Nothing,committedVisible=False,picker=Nothing,failure=Nothing,feedback=Just "下書きを破棄しました。"})
  PreviewDraft -> do
    m <- get
    mapM_ (\snap -> send (Previewing (currentVersion m) (workingData m))
      ["tag" J..= ("preview" :: S),"source" J..= snapshotRevision snap,"data" J..= workingData m]) (snapshot m)
  ApplyDraft -> do
    m <- get
    case (snapshot m,activeSystem m) of
      (Just snap,Just system) | verified m -> send Applying
        ["tag" J..= ("apply" :: S),"system" J..= system,"source" J..= snapshotRevision snap,"data" J..= workingData m
        ,"reason" J..= (if blank (reasonInput m) then "Studioでレコードを編集" else trim (reasonInput m))]
      _ -> localFailure "下書きを確認し、現在の確定版に対する変更として適用してください。"
  AppendPath arrow -> modify (\m -> m {focusPath=focusPath m++[arrow]})
  ClearPath -> modify (\m -> m {focusPath=[]})
  ExplainRow ref -> do
    m <- get
    Fetch.postJSON' "/api/command" (J.object ["tag" J..= ("explain" :: S),"ref" J..= ref]) (headers m)
      (GotExplanation ref . Fetch.body) ignoreJSONFailure
  GotExplanation ref reply -> modify $ \m -> if focusedRow m==Just (refRecord ref) && focusedObject m==refObject ref
      && maybe False ((==refRevision ref) . snapshotRevision) (snapshot m)
    then m {explanation=replyExplanation reply} else m
  AddObject -> do
    m <- get
    let key=trim (newObjectInput m); def=schemaDraft m
    if blank key || key `elem` schemaObjects def then localFailure "重複しないオブジェクト名を入力してください。"
      else modify (\x -> x {schemaDraft=def {schemaObjects=schemaObjects def++[key]},newObjectInput=""})
  RemoveObject key -> modify (\m -> m {schemaDraft=(schemaDraft m) {schemaObjects=filter (/=key) (schemaObjects (schemaDraft m))}})
  AddArrow -> do
    m <- get
    let key=trim (arrowNameInput m); def=schemaDraft m
    if blank key || blank (arrowFromInput m) || blank (arrowToInput m) || any ((==key) . arrowId) (schemaArrows def)
      then localFailure "矢印の名前・始点・終点を指定してください。名前はスキーマ内で一意にします。"
      else modify (\x -> x {schemaDraft=def {schemaArrows=schemaArrows def++[WArrow key (arrowFromInput m) (arrowToInput m)]},arrowNameInput=""})
  RemoveArrow key -> modify (\m -> m {schemaDraft=(schemaDraft m) {schemaArrows=filter ((/=key) . arrowId) (schemaArrows (schemaDraft m))}})
  AppendEquation side arrow -> modify (\m -> if side then m {equationLeftInput=equationLeftInput m++[arrow]} else m {equationRightInput=equationRightInput m++[arrow]})
  ClearEquation side -> modify (\m -> if side then m {equationLeftInput=[]} else m {equationRightInput=[]})
  AddEquation -> do
    m <- get
    let def=schemaDraft m; start=equationStartInput m
        a=WPath start (equationLeftInput m); b=WPath start (equationRightInput m)
    if blank start || pathEndObject def start (pathSteps a)/=pathEndObject def start (pathSteps b)
      then localFailure "二つの道は同じオブジェクトから始め、同じオブジェクトで終える必要があります。"
      else modify (\x -> x {schemaDraft=def {schemaEquations=schemaEquations def++[WEquation a b]},equationLeftInput=[],equationRightInput=[]})
  RemoveEquation index -> modify (\m -> m {schemaDraft=(schemaDraft m) {schemaEquations=[eq | (n,eq) <- zip [0..] (schemaEquations (schemaDraft m)),n/=index]}})
  SaveSchema -> do
    m <- get
    send SavingSchema ["tag" J..= ("schema" :: S),"name" J..= trim (schemaNameInput m),"schema" J..= schemaDraft m]
  ChooseMappingSchema source key -> modify $ \m ->
    let f=mappingDraft m in m {mappingDraft=if source then f {mappingFrom=key,mappingObjects=[],mappingArrows=[]}
      else f {mappingTo=key,mappingObjects=[],mappingArrows=[]},selectedMappingArrow=""}
  ChooseObjectImage object image -> modify (\m -> m {mappingDraft=(mappingDraft m) {mappingObjects=replace object image (mappingObjects (mappingDraft m))}})
  ChooseMappingArrow key -> modify (\m -> m {selectedMappingArrow=key})
  AppendMappingPath arrow -> modify $ \m ->
    let f=mappingDraft m; p=mappingSelectedPath m in m {mappingDraft=f {mappingArrows=replace (selectedMappingArrow m) p {pathSteps=pathSteps p++[arrow]} (mappingArrows f)}}
  ResetMappingPath -> modify $ \m ->
    let f=mappingDraft m; source=fromMaybe "" $ do s <- mappingInputSchema m; a <- arrowDef s (selectedMappingArrow m); lookup (arrowFrom a) (mappingObjects f)
    in m {mappingDraft=f {mappingArrows=replace (selectedMappingArrow m) (WPath source []) (mappingArrows f)}}
  SaveMapping -> do
    m <- get
    send SavingMapping ["tag" J..= ("mapping" :: S),"mapping" J..= (mappingDraft m) {mappingKey=trim (mappingKey (mappingDraft m))}]
  LoadMapping key -> do
    m <- get
    case snapshotState m >>= find ((==key) . mappingKey) . stateMappings of
      Nothing -> pure ()
      Just f -> modify (\x -> x {page=MappingEditor,mappingDraft=f {mappingKey=mappingKey f<>"-copy"},selectedMappingArrow=fromMaybe "" (fst <$> listToMaybe (mappingArrows f))})
  ChooseViewMapping key -> modify (\m -> m {viewMappingInput=key})
  RunView -> do
    m <- get
    let system=snapshotState m >>= fmap fst . find ((==viewSourceInput m) . snd) . stateHeads
    send ComputingView ["tag" J..= ("run" :: S),"name" J..= trim (viewNameInput m),"operation" J..= operationInput m
      ,"mapping" J..= viewMappingInput m,"source" J..= viewSourceInput m,"system" J..= system]
  RecomputeView key -> send ComputingView ["tag" J..= ("recompute" :: S),"name" J..= key]
  GeneratePart -> do
    m <- get
    let used=maybe [] (map viewKey . stateViews) (snapshotState m)
        partName=fromMaybe "part" (listToMaybe ["part-"<>ms n | n <- [1::Int ..],("part-"<>ms n) `notElem` used])
    case (snapshot m,focusedRow m) of
      (Just snap,Just key) -> send ComputingView ["tag" J..= ("generate" :: S),"name" J..= partName
        ,"source" J..= snapshotRevision snap,"object" J..= focusedObject m,"record" J..= key]
      _ -> pure ()
  AdoptRevision -> do
    m <- get
    mapM_ (\snap -> send Other ["tag" J..= ("adopt" :: S),"name" J..= trim (adoptNameInput m),"source" J..= snapshotRevision snap]) (snapshot m)
  CreateDatabase -> do
    m <- get
    send ChangingFile ["tag" J..= ("create" :: S),"path" J..= fileInput m,"preset" J..= presetInput m]
  OpenDatabase -> do
    m <- get
    send ChangingFile ["tag" J..= ("open" :: S),"path" J..= fileInput m]
  CloseDatabase -> send ChangingFile ["tag" J..= ("close" :: S)]
  AdoptLegacy -> do
    m <- get
    send ChangingFile ["tag" J..= ("adoptLegacy" :: S),"path" J..= fileInput m,"name" J..= (if blank (adoptNameInput m) then "imported" else trim (adoptNameInput m))]
  BackupDatabase -> do
    m <- get
    send Other ["tag" J..= ("backup" :: S),"path" J..= backupInput m]
  Download kind -> io (Downloaded <$> Browser.download kind)
  Downloaded message -> modify (\m -> m {feedback=Just (if blank message then "ダウンロードを開始しました。" else message)})
  Completed number purpose reply -> do
    m <- get
    when (number==sequenceNumber m) $ do
      modify (\x -> x {pending=Nothing,snapshotState=case replyState reply of Nothing -> snapshotState x; s -> s
        ,feedback=Just (replyMessage reply),failure=replyProblem reply})
      if not (replyOk reply)
        then when (case purpose of Previewing _ _ -> True; _ -> False) (modify (\x -> x {validated=Nothing}))
        else case purpose of
          Previewing ver dat -> modify (\x -> x {validated=if currentVersion x==ver && workingData x==dat then Just (ver,dat) else Nothing})
          Inspecting _ focus view -> do
            mapM_ (\snap -> modify (acceptSnapshot focus view snap)) (replySnapshot reply)
            when (snapshot m==Nothing && isJust (fieldCourse m)) (modify (\x->x {page=Course}))
            explainFocused
          GuideInspecting object key -> do
            forM_ (replySnapshot reply) $ \snap -> do
              let selected=case key of Just k->Just k; Nothing->recordId <$> listToMaybe (records (snapshotData snap) object)
                  focus=fmap (WRef (snapshotRevision snap) object) selected
              modify (acceptSnapshot focus Nothing snap)
            explainFocused
          GuideDrafting kind -> forM_ (replySnapshot reply) $ \snap -> do
            let rev=snapshotRevision snap
            if kind=="qualification" then do
              let key="資格13｜佐藤・センサー"
                  fields=[("qualificationEngineer","佐藤"),("qualificationProduct","センサー")]
              if isJust (record (snapshotData snap) "資格" key)
                then localFailure "この資格は既に原本にあります。別の行を普通の編集画面から追加できます。"
                else do
                  modify (acceptSnapshot (Just (WRef rev "資格" key)) Nothing snap)
                  modify ((\x->x {guideExercise=Just kind}) . changeData (map (\t->if tableObject t=="資格" then t {tableRows=tableRows t++[WRow key fields]} else t)))
            else do
              let actualProduct=lastNode =<< either (const Nothing) Just (follow (snapshotSchema snap) (snapshotData snap) (WPath "依頼" ["ticketContract","contractProduct"]) "依頼01｜青葉の定期点検")
                  proposed=if fmap snd actualProduct==Just "センサー" then "ポンプ" else "センサー"
              modify (acceptSnapshot (Just (WRef rev "依頼" "依頼01｜青葉の定期点検")) Nothing snap)
              modify ((\x->x {guideExercise=Just "broken"}) . changeData (setTarget "依頼" "依頼01｜青葉の定期点検" "ticketProduct" proposed))
          ChangingFile -> do
            modify (\x -> x {snapshot=Nothing,draft=Nothing,validated=Nothing,selectedView=Nothing,focusedRow=Nothing,explanation=Nothing,page=Files})
            initializeSelection
          SavingMapping -> modify (\x -> x {page=Mappings})
          _ -> do
            case replySnapshot reply of
              Nothing -> pure ()
              Just snap -> do
                state <- get
                let view=case purpose of
                      ComputingView -> snapshotState state >>= fmap viewKey . find ((==snapshotRevision snap) . viewOutput) . stateViews
                      FieldComputing _ -> snapshotState state >>= fmap viewKey . find ((==snapshotRevision snap) . viewOutput) . stateViews
                      _ -> Nothing
                modify (acceptSnapshot Nothing view snap)
                explainFocused
            when (purpose==Applying) (modify (\x -> x {reasonInput="",guideExercise=Nothing,page=if isJust (fieldCourse x) then Course else page x}))
  CommandFailed number msg -> modify (\m -> if number==sequenceNumber m then m {pending=Nothing,feedback=Just msg} else m)
  CancelWork -> do
    m <- get
    Fetch.postJSON' "/api/command" (J.object ["tag" J..= ("cancel" :: S)]) (headers m)
      (CancelledReply . Fetch.body) ignoreJSONFailure
  CancelledReply reply -> modify (\m -> m {feedback=Just (replyMessage reply),failure=replyProblem reply})

headers :: Model -> [(S,S)]
headers m = [("X-CatDB-Session",token m)]

stateFailure :: Fetch.Response S -> Action
stateFailure = NetworkFailed . fromMaybe "サーバーに接続できません。起動時のURLを開き直してください。" . Fetch.errorMessage
ignoreJSONFailure :: Fetch.Response J.Value -> Action
ignoreJSONFailure _ = Noop

send :: Purpose -> [J.Pair] -> Effect parent props Model Action
send purpose pairs = do
  m <- get
  unless (isPending m) $ do
    let number=sequenceNumber m+1
    modify (\x -> x {pending=Just purpose,sequenceNumber=number,feedback=Nothing,failure=Nothing})
    Fetch.postJSON' "/api/command" (J.object (("expected" J..= currentVersion m):pairs)) (headers m)
      (Completed number purpose . Fetch.body) (\response -> CommandFailed number $ case J.fromJSON (Fetch.body response) of
        J.Success reply -> replyMessage (reply :: WReply)
        J.Error _ -> fromMaybe "操作を完了できませんでした。接続と入力を確認してください。" (Fetch.errorMessage response))

localFailure :: S -> Effect parent props Model Action
localFailure message = modify (\m -> m {feedback=Just message,failure=Just (WProblem "Input" message "")})

initializeSelection :: Effect parent props Model Action
initializeSelection = do
  m <- get
  case snapshotState m of
    Nothing -> pure ()
    Just state -> case stateLegacy state of
      Just snap -> modify (acceptSnapshot Nothing Nothing snap)
      Nothing -> case listToMaybe (stateHeads state) of
        Just (_,rev) -> issue (OpenRevision rev Nothing Nothing)
        Nothing -> when (isJust (statePath state)) (modify (\x -> x {page=SchemaEditor}))

explainFocused :: Effect parent props Model Action
explainFocused = do
  m <- get
  case (snapshot m,focusedRow m) of
    (Just snap,Just key) | not (dirty m) && snapshotRevision snap/="legacy" -> issue (ExplainRow (WRef (snapshotRevision snap) (focusedObject m) key))
    _ -> pure ()
