module Model where

import Data.List (find)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
import Miso.String (MisoString, fromMisoString, ms)
import Studio.Wire
import DiagramLayout (Point(..),pathNodes)

type S = MisoString
data Page = Course | Records | Rules | Structure | SchemaEditor | Mappings | MappingEditor | ViewBuilder | Files | History deriving (Eq, Show)
data Field = FilePathField | PresetField | BackupField | ReasonField | SearchField | NewRecordField
  | SchemaNameField | NewObjectField | ArrowNameField | ArrowFromField | ArrowToField | EquationStartField
  | MappingNameField | ViewNameField | OperationField | ViewSourceField | AdoptNameField
  deriving (Eq,Show)
data Purpose = Inspecting S (Maybe WRef) (Maybe S) | GuideInspecting S (Maybe S) | GuideDrafting S | FieldComputing S
  | Previewing S WData | CheckingSchema S WSchema | CheckingMapping WMapping
  | Applying | SavingSchema | SavingMapping | ComputingView | ChangingFile | Other
  deriving (Eq, Show)
data DiagramSelection = SelectObject S | SelectArrow S | SelectEquation Int deriving (Eq,Show)
data DiagramTool = InspectTool | ConnectTool (Maybe S) | EquationTool Bool | ObjectMapTool (Maybe S) deriving (Eq,Show)
data Gesture = PanGesture Point Point | MoveGesture S Point Point deriving (Eq,Show)
data Action
  = Initialize | Initialized S | Poll | GotState WState | NetworkFailed S
  | Navigate Page | Edit Field S | ToggleMath | Dismiss | Noop
  | Guard Action | ConfirmDiscard | KeepDraft
  | OpenRevision S (Maybe WRef) (Maybe S) | FocusObject S | FocusRow S S
  | Pick S S S | ClosePicker | ChooseTarget S | SetInsertTarget S S | AddRecord | DeleteRecord
  | ShowCommitted Bool | DiscardDraft | PreviewDraft | ApplyDraft
  | AppendPath S | ClearPath | ExplainRow WRef | GotExplanation WRef WReply
  | AddObject | RemoveObject S | AddArrow | RemoveArrow S
  | AppendEquation Bool S | ClearEquation Bool | AddEquation | RemoveEquation Int | SaveSchema
  | ChooseMappingSchema Bool S | ChooseObjectImage S S | ChooseMappingArrow S
  | AppendMappingPath S | ResetMappingPath | SaveMapping | LoadMapping S
  | ChooseViewMapping S | RunView | RecomputeView S | GeneratePart | AdoptRevision
  | CreateDatabase | OpenDatabase | CloseDatabase | AdoptLegacy | BackupDatabase | Download S | Downloaded S
  | Completed Int Purpose WReply | CommandFailed Int S | CancelWork | CancelledReply WReply
  | ToggleGuideTools | GuideOpenSource S (Maybe S) | GuideMakeDraft S | GuideRepair | FieldStage S
  | SelectDiagram DiagramSelection | SetDiagramTool DiagramTool | DiagramObject Bool S | DiagramArrow Bool S
  | StartSchema Bool | BrowseSchema S | CheckSchema | CheckMapping | UseMapping S
  | ZoomDiagram Double | FitDiagram | RearrangeDiagram | PanDiagram Double Double | NudgeNode Double Double
  | BeginPan Double Double | BeginMove S Point Double Double | MovePointer Double Double | EndGesture
  deriving (Eq, Show)

data Model = Model
  { snapshotState :: Maybe WState, snapshot :: Maybe WSnapshot, draft :: Maybe WData
  , validated :: Maybe (S,WData), committedVisible :: Bool, page :: Page
  , focusedObject :: S, focusedRow :: Maybe S, focusPath :: [S], selectedView :: Maybe S
  , picker :: Maybe (S,S,S), explanation :: Maybe WExplanation
  , token :: S, polling :: Bool, pending :: Maybe Purpose, sequenceNumber :: Int
  , feedback :: Maybe S, failure :: Maybe WProblem, confirm :: Maybe Action, math :: Bool
  , search :: S, fileInput :: S, presetInput :: S, backupInput :: S, reasonInput :: S, newRecordInput :: S
  , insertTargets :: [(S,S)], schemaNameInput :: S, schemaDraft :: WSchema, newObjectInput :: S
  , arrowNameInput :: S, arrowFromInput :: S, arrowToInput :: S, equationStartInput :: S
  , equationLeftInput :: [S], equationRightInput :: [S], mappingDraft :: WMapping, selectedMappingArrow :: S
  , viewNameInput :: S, operationInput :: S, viewSourceInput :: S, viewMappingInput :: S, adoptNameInput :: S
  , guideTools :: Bool, guideExercise :: Maybe S
  , diagramSelection :: Maybe DiagramSelection, diagramTool :: DiagramTool, browsedSchema :: S
  , diagramPositions :: [(S,Point)], diagramZoom :: Double, diagramPan :: Point, diagramGesture :: Maybe Gesture
  , schemaValidated :: Maybe (S,S,WSchema), mappingValidated :: Maybe (S,WMapping)
  } deriving (Eq,Show)

initialModel :: Model
initialModel = Model
  { snapshotState=Nothing,snapshot=Nothing,draft=Nothing,validated=Nothing,committedVisible=False,page=Files
  , focusedObject="",focusedRow=Nothing,focusPath=[],selectedView=Nothing,picker=Nothing,explanation=Nothing
  , token="",polling=False,pending=Nothing,sequenceNumber=0,feedback=Nothing,failure=Nothing,confirm=Nothing,math=False
  , search="",fileInput="読書メモ.catdb",presetInput="reading",backupInput="バックアップ.catdb",reasonInput="",newRecordInput=""
  , insertTargets=[],schemaNameInput="",schemaDraft=WSchema [] [] [],newObjectInput="",arrowNameInput="",arrowFromInput="",arrowToInput=""
  , equationStartInput="",equationLeftInput=[],equationRightInput=[],mappingDraft=WMapping "" "" "" [] [],selectedMappingArrow=""
  , viewNameInput="",operationInput="delta",viewSourceInput="",viewMappingInput="",adoptNameInput=""
  , guideTools=False,guideExercise=Nothing
  , diagramSelection=Just (SelectEquation 0),diagramTool=InspectTool,browsedSchema=""
  , diagramPositions=[],diagramZoom=1,diagramPan=Point 0 0,diagramGesture=Nothing
  , schemaValidated=Nothing,mappingValidated=Nothing
  }

trim :: S -> S
trim = ms . T.strip . fromMisoString
blank :: S -> Bool
blank = (=="") . trim
currentVersion :: Model -> S
currentVersion = maybe "" stateVersion . snapshotState
workingData :: Model -> WData
workingData m = fromMaybe (maybe [] snapshotData (snapshot m)) (draft m)
visibleData :: Model -> WData
visibleData m = if committedVisible m then maybe [] snapshotData (snapshot m) else workingData m
dirty :: Model -> Bool
dirty m = maybe False (\dat -> Just dat /= (snapshotData <$> snapshot m)) (draft m)
verified :: Model -> Bool
verified m = validated m == Just (currentVersion m,workingData m) && dirty m
isPending :: Model -> Bool
isPending = maybe False (const True) . pending
activeSystem :: Model -> Maybe S
activeSystem m = do
  s <- snapshotState m
  snap <- snapshot m
  fst <$> find ((== snapshotRevision snap) . snd) (stateHeads s)
editingAllowed :: Model -> Bool
editingAllowed m = activeSystem m /= Nothing && selectedView m == Nothing && not (committedVisible m)
currentSchema :: Model -> WSchema
currentSchema = maybe (WSchema [] [] []) snapshotSchema . snapshot
outgoing :: WSchema -> S -> [WArrow]
outgoing schema object = filter ((==object) . arrowFrom) (schemaArrows schema)
records :: WData -> S -> [WRow]
records dat object = maybe [] tableRows (find ((==object) . tableObject) dat)
record :: WData -> S -> S -> Maybe WRow
record dat object key = find ((==key) . recordId) (records dat object)
fieldTarget :: WData -> S -> S -> S -> Maybe S
fieldTarget dat object key arrow = record dat object key >>= lookup arrow . recordFields
arrowDef :: WSchema -> S -> Maybe WArrow
arrowDef schema key = find ((==key) . arrowId) (schemaArrows schema)
pathEndObject :: WSchema -> S -> [S] -> Maybe S
pathEndObject schema object steps = either (const Nothing) (listToMaybe . reverse)
  (pathNodes [(arrowId a,arrowFrom a,arrowTo a)|a<-schemaArrows schema] object steps)

-- The client proposes and draws. Only the native engine admits a new revision.
type Node = (S,S)
follow :: WSchema -> WData -> WPath -> S -> Either S [Node]
follow schema dat (WPath start arrows) key = go start key arrows
  where
    go o x rest = case record dat o x of
      Nothing -> Left (o <> " / " <> x <> " がありません")
      Just _ -> case rest of
        [] -> Right [(o,x)]
        a:as -> case arrowDef schema a of
          Just def | arrowFrom def == o -> case fieldTarget dat o x a of
            Nothing -> Left (a <> " が未指定です")
            Just y -> ((o,x):) <$> go (arrowTo def) y as
          _ -> Left (a <> " はこの位置から進めません")
lastNode :: [Node] -> Maybe Node
lastNode = listToMaybe . reverse
equationCheck :: WSchema -> WData -> WEquation -> S -> (Either S [Node],Either S [Node],Bool)
equationCheck schema dat eq key = (a,b,case (a,b) of (Right x,Right y) -> lastNode x==lastNode y; _ -> False)
  where a=follow schema dat (equationLeft eq) key; b=follow schema dat (equationRight eq) key
ruleChecks :: WSchema -> WData -> [(Int,WEquation,S,Bool)]
ruleChecks schema dat = [(n,eq,recordId r,ok) | (n,eq) <- zip [0..] (schemaEquations schema),r <- records dat (pathStart (equationLeft eq))
  ,let (_,_,ok)=equationCheck schema dat eq (recordId r)]
localProblems :: WSchema -> WData -> Int
localProblems schema dat = length (filter (not . fourth) (ruleChecks schema dat)) + length
  [() | WTable o rs <- dat,WRow x _ <- rs,a <- outgoing schema o
      ,maybe True (\y -> record dat (arrowTo a) y==Nothing) (fieldTarget dat o x (arrowId a))]
  where fourth (_,_,_,v)=v
pathLabel :: WPath -> S
pathLabel (WPath o []) = "id(" <> o <> ")"
pathLabel (WPath _ as) = ms (T.intercalate " → " (map fromMisoString as))

changeData :: (WData -> WData) -> Model -> Model
changeData change m = m {draft=Just (change (workingData m)),validated=Nothing,committedVisible=False,failure=Nothing,feedback=Nothing}
setTarget :: S -> S -> S -> S -> WData -> WData
setTarget o key arrow target = map $ \t -> if tableObject t/=o then t else t {tableRows=map update (tableRows t)}
  where update r = if recordId r/=key then r else r {recordFields=replace arrow target (recordFields r)}
replace :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
replace key value pairs = if any ((==key) . fst) pairs then map (\(k,v) -> (k,if k==key then value else v)) pairs else pairs++[(key,value)]
acceptSnapshot :: Maybe WRef -> Maybe S -> WSnapshot -> Model -> Model
acceptSnapshot focus view snap m = m
  { snapshot=Just snap,draft=Nothing,validated=Nothing,committedVisible=False,selectedView=view,page=Records
  , focusedObject=object,focusedRow=key,focusPath=[],picker=Nothing,explanation=Nothing,search="",insertTargets=[]
  , viewSourceInput=snapshotRevision snap,viewMappingInput=""
  , browsedSchema=snapshotSchemaKey snap,diagramSelection=Just (SelectEquation 0),diagramTool=InspectTool
  , diagramZoom=1,diagramPan=Point 0 0
  }
  where
    object=maybe (fromMaybe "" (listToMaybe (schemaObjects (snapshotSchema snap)))) refObject focus
    key=case focus of Just r -> Just (refRecord r); Nothing -> recordId <$> listToMaybe (records (snapshotData snap) object)

editField :: Field -> S -> Model -> Model
editField field value m = case field of
  FilePathField -> m {fileInput=value}
  PresetField -> m {presetInput=value,fileInput=case value of "reading" -> "読書メモ.catdb"; "families" -> "対応の見本.catdb"; "field-service" -> "現場サービス.catdb"; _ -> "新しいデータベース.catdb"}
  BackupField -> m {backupInput=value}
  ReasonField -> m {reasonInput=value}
  SearchField -> m {search=value}
  NewRecordField -> m {newRecordInput=value}
  SchemaNameField -> m {schemaNameInput=value}
  NewObjectField -> m {newObjectInput=value}
  ArrowNameField -> m {arrowNameInput=value}
  ArrowFromField -> m {arrowFromInput=value}
  ArrowToField -> m {arrowToInput=value}
  EquationStartField -> m {equationStartInput=value,equationLeftInput=[],equationRightInput=[]}
  MappingNameField -> m {mappingDraft=(mappingDraft m) {mappingKey=value}}
  ViewNameField -> m {viewNameInput=value}
  OperationField -> m {operationInput=value,viewMappingInput=""}
  ViewSourceField -> m {viewSourceInput=value,viewMappingInput=""}
  AdoptNameField -> m {adoptNameInput=value}

schemaByKey :: Model -> S -> Maybe WSchema
schemaByKey m key = snapshotState m >>= lookup key . stateSchemas
mappingInputSchema :: Model -> Maybe WSchema
mappingInputSchema m = schemaByKey m (mappingFrom (mappingDraft m))
mappingOutputSchema :: Model -> Maybe WSchema
mappingOutputSchema m = schemaByKey m (mappingTo (mappingDraft m))
mappingSelectedPath :: Model -> WPath
mappingSelectedPath m = fromMaybe (WPath source []) (lookup (selectedMappingArrow m) (mappingArrows f))
  where f=mappingDraft m; source=fromMaybe "" $ do s <- mappingInputSchema m; a <- arrowDef s (selectedMappingArrow m); lookup (arrowFrom a) (mappingObjects f)
eligibleMappings :: Model -> [WMapping]
eligibleMappings m = case snapshotState m of
  Nothing -> []
  Just state -> case find ((==viewSourceInput m) . revisionId) (stateRevisions state) of
    Nothing -> []
    Just source -> filter (\f -> (if operationInput m=="delta" then mappingTo f else mappingFrom f)==revisionSchema source) (stateMappings state)

caption :: Model -> S -> S -> S
caption m object key = fromMaybe fallback $ do
  snap <- snapshot m
  course <- fieldCourse m
  chosen <- find ((==WRef (snapshotRevision snap) object key) . dispatchRow) (concatMap courseRunRows (courseRuns course))
  pure (dispatchTicket chosen<>" · "<>dispatchEngineer chosen)
  where fallback=fromMaybe key $ do snap<-snapshot m; labels<-lookup object (snapshotLabels snap); lookup key labels

fieldCourse :: Model -> Maybe WCourse
fieldCourse m=snapshotState m >>= stateCourse
fieldStageName :: Model -> S
fieldStageName m=case snapshotSchemaKey <$> snapshot m of
  Just "fs:raw"->"原本"; Just "fs:normalized"->"Σ 顧客をそろえる"; Just "fs:operations"->"Δ 派遣の形で読む"; Just "fs:candidates"->"Π 担当できる組"; _->""
fieldView :: S -> Model -> Maybe WView
fieldView name m=snapshotState m >>= find ((==name) . viewKey) . stateViews

displayedSchema :: Model -> WSchema
displayedSchema m | page m==SchemaEditor=schemaDraft m
                  | otherwise=fromMaybe (currentSchema m) (schemaByKey m (browsedSchema m))
schemaChecked :: Model -> Bool
schemaChecked m=schemaValidated m==Just (currentVersion m,trim (schemaNameInput m),schemaDraft m)
mappingChecked :: Model -> Bool
mappingChecked m=mappingValidated m==Just (currentVersion m,mappingDraft m)
diagramScope :: Model -> S
diagramScope m | page m==Records="lens:"<>maybe "" snapshotRevision (snapshot m)
               | page m `elem` [MappingEditor,ViewBuilder]="mapping:"
               | page m==SchemaEditor="draft:"
               | otherwise="schema:"<>browsedSchema m<>":"
