{-# LANGUAGE CPP, OverloadedStrings #-}
module Studio.Wire where

#ifdef WASM
import Miso.JSON
import Miso.String (MisoString)
type WireText = MisoString
#else
import Data.Aeson
import Data.Text (Text)
type WireText = Text
#endif

data WPath = WPath { pathStart :: WireText, pathSteps :: [WireText] } deriving (Eq, Show)
data WArrow = WArrow { arrowId :: WireText, arrowFrom :: WireText, arrowTo :: WireText } deriving (Eq, Show)
data WEquation = WEquation { equationLeft :: WPath, equationRight :: WPath } deriving (Eq, Show)
data WSchema = WSchema { schemaObjects :: [WireText], schemaArrows :: [WArrow], schemaEquations :: [WEquation] } deriving (Eq, Show)
data WRow = WRow { recordId :: WireText, recordFields :: [(WireText,WireText)] } deriving (Eq, Show)
data WTable = WTable { tableObject :: WireText, tableRows :: [WRow] } deriving (Eq, Show)
type WData = [WTable]
data WMapping = WMapping
  { mappingKey :: WireText, mappingFrom :: WireText, mappingTo :: WireText
  , mappingObjects :: [(WireText,WireText)], mappingArrows :: [(WireText,WPath)] } deriving (Eq, Show)
data WRevision = WRevision
  { revisionId :: WireText, revisionSchema :: WireText, revisionDerived :: Bool
  , revisionPrevious :: Maybe WireText, revisionReason :: WireText, revisionCount :: Int } deriving (Eq, Show)
data WView = WView
  { viewKey :: WireText, viewOperation :: WireText, viewMapping :: WireText
  , viewInput :: WireText, viewOutput :: WireText, viewSystem :: Maybe WireText
  , viewFreshness :: WireText, viewCapturedSource :: Maybe WireText } deriving (Eq, Show)
data WSnapshot = WSnapshot
  { snapshotRevision :: WireText, snapshotSchemaKey :: WireText, snapshotSchema :: WSchema
  , snapshotData :: WData, snapshotDerived :: Bool, snapshotLabels :: [(WireText,[(WireText,WireText)])] } deriving (Eq, Show)
data WState = WState
  { stateVersion :: WireText, statePath :: Maybe WireText, stateHeads :: [(WireText,WireText)]
  , stateSchemas :: [(WireText,WSchema)], stateRevisions :: [WRevision]
  , stateMappings :: [WMapping], stateViews :: [WView], stateLegacy :: Maybe WSnapshot
  , stateBusy :: Bool, stateCourse :: Maybe WCourse } deriving (Eq, Show)
data WRef = WRef { refRevision :: WireText, refObject :: WireText, refRecord :: WireText } deriving (Eq, Show)
data WOrigin = WOrigin { originRef :: WRef, originPath :: Maybe WPath, originSlot :: Maybe WireText } deriving (Eq, Show)
data WExplanation = WExplanation
  { explanationRef :: WRef, explanationKind :: WireText, explanationOrigins :: [WOrigin]
  , explanationAnchors :: [WireText] } deriving (Eq, Show)
data WDispatch = WDispatch
  { dispatchRow :: WRef, dispatchTicket :: WireText, dispatchEngineer :: WireText
  , dispatchQualification :: WireText, dispatchAvailability :: WireText
  , dispatchProduct :: WireText, dispatchSlot :: WireText, dispatchRegion :: WireText
  , dispatchOrigins :: [WRef], dispatchProjections :: [(WireText,WRef)] } deriving (Eq, Show)
data WCourseRun = WCourseRun
  { courseRunRevision :: WireText, courseRunSource :: WireText, courseRunRows :: [WDispatch] } deriving (Eq, Show)
data WCondition = WCondition
  { conditionName :: WireText, conditionLeft :: WireText, conditionRight :: WireText, conditionMatches :: Bool } deriving (Eq, Show)
data WCourse = WCourse
  { courseSource :: WireText, courseCounts :: [(WireText,Int)], courseRuns :: [WCourseRun]
  , courseTicketCounts :: [(WireText,Int)], courseNearMiss :: [WCondition] } deriving (Eq, Show)
data WProblem = WProblem { problemCode :: WireText, problemMessage :: WireText, problemDetail :: WireText } deriving (Eq, Show)
data WReply = WReply
  { replyOk :: Bool, replyMessage :: WireText, replyProblem :: Maybe WProblem
  , replyState :: Maybe WState, replySnapshot :: Maybe WSnapshot
  , replyExplanation :: Maybe WExplanation } deriving (Eq, Show)

instance ToJSON WPath where toJSON (WPath a b) = object ["start" .= a,"steps" .= b]
instance FromJSON WPath where parseJSON = withObject "path" $ \v -> WPath <$> v .: "start" <*> v .: "steps"
instance ToJSON WArrow where toJSON (WArrow a b c) = object ["id" .= a,"from" .= b,"to" .= c]
instance FromJSON WArrow where parseJSON = withObject "arrow" $ \v -> WArrow <$> v .: "id" <*> v .: "from" <*> v .: "to"
instance ToJSON WEquation where toJSON (WEquation a b) = object ["left" .= a,"right" .= b]
instance FromJSON WEquation where parseJSON = withObject "equation" $ \v -> WEquation <$> v .: "left" <*> v .: "right"
instance ToJSON WSchema where toJSON (WSchema a b c) = object ["objects" .= a,"arrows" .= b,"equations" .= c]
instance FromJSON WSchema where parseJSON = withObject "schema" $ \v -> WSchema <$> v .: "objects" <*> v .: "arrows" <*> v .: "equations"
instance ToJSON WRow where toJSON (WRow a b) = object ["id" .= a,"fields" .= b]
instance FromJSON WRow where parseJSON = withObject "record" $ \v -> WRow <$> v .: "id" <*> v .: "fields"
instance ToJSON WTable where toJSON (WTable a b) = object ["object" .= a,"rows" .= b]
instance FromJSON WTable where parseJSON = withObject "table" $ \v -> WTable <$> v .: "object" <*> v .: "rows"
instance ToJSON WMapping where toJSON (WMapping a b c d e) = object ["key" .= a,"from" .= b,"to" .= c,"objects" .= d,"arrows" .= e]
instance FromJSON WMapping where parseJSON = withObject "mapping" $ \v -> WMapping <$> v .: "key" <*> v .: "from" <*> v .: "to" <*> v .: "objects" <*> v .: "arrows"
instance ToJSON WRevision where toJSON (WRevision a b c d e f) = object ["id" .= a,"schema" .= b,"derived" .= c,"previous" .= d,"reason" .= e,"count" .= f]
instance FromJSON WRevision where parseJSON = withObject "revision" $ \v -> WRevision <$> v .: "id" <*> v .: "schema" <*> v .: "derived" <*> v .: "previous" <*> v .: "reason" <*> v .: "count"
instance ToJSON WView where toJSON (WView a b c d e f g h) = object ["key" .= a,"operation" .= b,"mapping" .= c,"input" .= d,"output" .= e,"system" .= f,"freshness" .= g,"capturedSource" .= h]
instance FromJSON WView where parseJSON = withObject "view" $ \v -> WView <$> v .: "key" <*> v .: "operation" <*> v .: "mapping" <*> v .: "input" <*> v .: "output" <*> v .: "system" <*> v .: "freshness" <*> v .:? "capturedSource"
instance ToJSON WSnapshot where toJSON (WSnapshot a b c d e f) = object ["revision" .= a,"schemaKey" .= b,"schema" .= c,"data" .= d,"derived" .= e,"labels" .= f]
instance FromJSON WSnapshot where parseJSON = withObject "snapshot" $ \v -> WSnapshot <$> v .: "revision" <*> v .: "schemaKey" <*> v .: "schema" <*> v .: "data" <*> v .: "derived" <*> v .:? "labels" .!= []
instance ToJSON WState where toJSON (WState a b c d e f g h i j) = object ["version" .= a,"path" .= b,"heads" .= c,"schemas" .= d,"revisions" .= e,"mappings" .= f,"views" .= g,"legacy" .= h,"busy" .= i,"course" .= j]
instance FromJSON WState where parseJSON = withObject "state" $ \v -> WState <$> v .: "version" <*> v .: "path" <*> v .: "heads" <*> v .: "schemas" <*> v .: "revisions" <*> v .: "mappings" <*> v .: "views" <*> v .: "legacy" <*> v .: "busy" <*> v .:? "course"
instance ToJSON WRef where toJSON (WRef a b c) = object ["revision" .= a,"object" .= b,"record" .= c]
instance FromJSON WRef where parseJSON = withObject "reference" $ \v -> WRef <$> v .: "revision" <*> v .: "object" <*> v .: "record"
instance ToJSON WOrigin where toJSON (WOrigin a b c) = object ["ref" .= a,"path" .= b,"slot" .= c]
instance FromJSON WOrigin where parseJSON = withObject "origin" $ \v -> WOrigin <$> v .: "ref" <*> v .: "path" <*> v .: "slot"
instance ToJSON WExplanation where toJSON (WExplanation a b c d) = object ["ref" .= a,"kind" .= b,"origins" .= c,"anchors" .= d]
instance FromJSON WExplanation where parseJSON = withObject "explanation" $ \v -> WExplanation <$> v .: "ref" <*> v .: "kind" <*> v .: "origins" <*> v .: "anchors"
instance ToJSON WDispatch where toJSON (WDispatch a b c d e f g h i j) = object ["row" .= a,"ticket" .= b,"engineer" .= c,"qualification" .= d,"availability" .= e,"product" .= f,"slot" .= g,"region" .= h,"origins" .= i,"projections" .= j]
instance FromJSON WDispatch where parseJSON = withObject "dispatch" $ \v -> WDispatch <$> v .: "row" <*> v .: "ticket" <*> v .: "engineer" <*> v .: "qualification" <*> v .: "availability" <*> v .: "product" <*> v .: "slot" <*> v .: "region" <*> v .: "origins" <*> v .: "projections"
instance ToJSON WCourseRun where toJSON (WCourseRun a b c) = object ["revision" .= a,"source" .= b,"rows" .= c]
instance FromJSON WCourseRun where parseJSON = withObject "course run" $ \v -> WCourseRun <$> v .: "revision" <*> v .: "source" <*> v .: "rows"
instance ToJSON WCondition where toJSON (WCondition a b c d) = object ["name" .= a,"left" .= b,"right" .= c,"matches" .= d]
instance FromJSON WCondition where parseJSON = withObject "condition" $ \v -> WCondition <$> v .: "name" <*> v .: "left" <*> v .: "right" <*> v .: "matches"
instance ToJSON WCourse where toJSON (WCourse a b c d e) = object ["source" .= a,"counts" .= b,"runs" .= c,"ticketCounts" .= d,"nearMiss" .= e]
instance FromJSON WCourse where parseJSON = withObject "course" $ \v -> WCourse <$> v .: "source" <*> v .: "counts" <*> v .: "runs" <*> v .: "ticketCounts" <*> v .: "nearMiss"
instance ToJSON WProblem where toJSON (WProblem a b c) = object ["code" .= a,"message" .= b,"detail" .= c]
instance FromJSON WProblem where parseJSON = withObject "problem" $ \v -> WProblem <$> v .: "code" <*> v .: "message" <*> v .: "detail"
instance ToJSON WReply where toJSON (WReply a b c d e f) = object ["ok" .= a,"message" .= b,"problem" .= c,"state" .= d,"snapshot" .= e,"explanation" .= f]
instance FromJSON WReply where parseJSON = withObject "reply" $ \v -> WReply <$> v .: "ok" <*> v .: "message" <*> v .: "problem" <*> v .: "state" <*> v .: "snapshot" <*> v .: "explanation"
