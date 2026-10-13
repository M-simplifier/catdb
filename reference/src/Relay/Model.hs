{-# LANGUAGE LambdaCase #-}
module Relay.Model
  ( workspace, demoWorkspace, Command, applyCommand, dashboard, publicExport, errorMessage ) where

import CatDB
import Control.Monad (forM, unless)
import Data.Aeson hiding (Object, Error, pairs)
import Data.Aeson.Types (Parser)
import qualified Data.ByteString.Lazy as L
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

data Roster = Roster { rId :: Text, rName :: Text, rEmail :: Text, rTeam :: Text, rSource :: Text }
data Ticket = Ticket { tId :: Text, tTitle :: Text, tTeam :: Text, tState :: Text }
data Command = ImportRoster Text Bool [Roster] | ImportTickets [Ticket]
  | PutTicket Ticket | Assign Text Text | Unassign Text | SetStatus Text Text
  | DeleteTicket Text | Link Text Text | Unlink Text

nonblank :: Text -> Parser Text
nonblank text = let s = T.strip text in
  if T.null s || T.length s > 2000 then fail "Fields must contain 1–2000 characters." else pure s

parseTicket :: Value -> Parser Ticket
parseTicket = withObject "request" $ \o -> do
  key <- o .: "id" >>= nonblank
  title <- o .: "title" >>= nonblank
  team <- o .: "team" >>= nonblank
  pure (Ticket key title team "open")

instance FromJSON Command where
  parseJSON = withObject "command" $ \o -> do
    action <- o .: "action" :: Parser Text
    case action of
      "importRoster" -> do
        src <- o .: "source" >>= nonblank
        replace <- o .:? "replace" .!= False
        inputs <- o .: "rows"
        rs <- forM inputs $ withObject "roster row" $ \r -> do
          name <- r .: "name" >>= nonblank
          email <- T.toCaseFold <$> (r .: "email" >>= nonblank)
          unless ("@" `T.isInfixOf` email && not (T.any (==' ') email)) (fail "Each roster row needs an email address.")
          team <- r .: "team" >>= nonblank
          key <- r .:? "id" .!= "" >>= \x -> if T.null (T.strip x) then pure (email <> "/" <> team) else nonblank x
          let escape = T.replace "/" "%2F" . T.replace "%" "%25"
          pure (Roster (escape src <> "/" <> escape key) name email team src)
        unless (not (null rs) && length rs <= 5000) (fail "Import 1–5000 roster rows at a time.")
        unless (length rs == S.size (S.fromList (map rId rs))) (fail "Duplicate row IDs. Use a distinct id column for each source row.")
        pure (ImportRoster src replace rs)
      "importTickets" -> do
        ts <- o .: "rows" >>= mapM parseTicket
        unless (not (null ts) && length ts <= 5000) (fail "Import 1–5000 requests at a time.")
        unless (length ts == S.size (S.fromList (map tId ts))) (fail "Duplicate request IDs.")
        pure (ImportTickets ts)
      "putTicket" -> PutTicket <$> (o .: "ticket" >>= parseTicket)
      "assign" -> Assign <$> (o .: "ticket" >>= nonblank) <*> (o .: "record" >>= nonblank)
      "unassign" -> Unassign <$> o .: "ticket"
      "status" -> do
        key <- o .: "ticket"
        state <- o .: "status"
        unless (state `elem` ["open","closed"]) (fail "Status must be open or closed.")
        pure (SetStatus key state)
      "deleteTicket" -> DeleteTicket <$> o .: "ticket"
      "link" -> Link <$> o .: "left" <*> o .: "right"
      "unlink" -> Unlink <$> o .: "id"
      _ -> fail "Unknown action."

workspace :: Either Error Instance
workspace = do
  s <- schema (SchemaDef ["Roster","Identity","Team","Source","Text","Ticket","Status","Assignment","Link"]
    [ ArrowDef "email" "Roster" "Identity", ArrowDef "name" "Roster" "Text"
    , ArrowDef "rosterTeam" "Roster" "Team", ArrowDef "origin" "Roster" "Source"
    , ArrowDef "ticketTeam" "Ticket" "Team", ArrowDef "title" "Ticket" "Text"
    , ArrowDef "status" "Ticket" "Status", ArrowDef "ticket" "Assignment" "Ticket"
    , ArrowDef "assignee" "Assignment" "Roster"
    , ArrowDef "left" "Link" "Roster", ArrowDef "right" "Link" "Roster" ]
    [Equation (Path "Assignment" ["ticket","ticketTeam"]) (Path "Assignment" ["assignee","rosterTeam"])])
  edit [Insert "Status" "open" M.empty,Insert "Status" "closed" M.empty] (empty s)

atomEdits :: Instance -> Object -> [Text] -> Either Error [Edit]
atomEdits i o values = do
  old <- elements i o
  pure [Insert o x M.empty | x <- S.toList (S.fromList (map Element values) S.\\ old)]

putRow :: Instance -> Object -> Text -> [(Arrow,Text)] -> Either Error Edit
putRow i o key fields = do
  old <- elements i o
  pure ((if Element key `S.member` old then Update else Insert) o (Element key)
         (M.fromList [(a,Element x) | (a,x) <- fields]))

field :: Instance -> Object -> Text -> [Arrow] -> Either Error Text
field i o key path = elementName <$> evaluate i (Path o path) (Element key)

rosterRows :: Instance -> Either Error [Roster]
rosterRows i = do
  keys <- elements i "Roster"
  forM (S.toAscList keys) $ \(Element k) ->
    Roster k <$> field i "Roster" k ["name"] <*> field i "Roster" k ["email"]
             <*> field i "Roster" k ["rosterTeam"] <*> field i "Roster" k ["origin"]

ticketRows :: Instance -> Either Error [Ticket]
ticketRows i = do
  keys <- elements i "Ticket"
  forM (S.toAscList keys) $ \(Element k) ->
    Ticket k <$> field i "Ticket" k ["title"] <*> field i "Ticket" k ["ticketTeam"] <*> field i "Ticket" k ["status"]

applyCommand :: Command -> Instance -> Either Error Instance
applyCommand command i = case command of
  ImportRoster src replace rs -> do
    atoms <- concat <$> sequence [atomEdits i "Text" (map rName rs),atomEdits i "Identity" (map rEmail rs),
                 atomEdits i "Team" (map rTeam rs),atomEdits i "Source" [src]]
    edits <- forM rs $ \r -> putRow i "Roster" (rId r)
      [("name",rName r),("email",rEmail r),("rosterTeam",rTeam r),("origin",src)]
    old <- rosterRows i
    let keep = S.fromList (map rId rs)
        removed = [Delete "Roster" (Element (rId r)) | r <- old, replace, rSource r == src, rId r `S.notMember` keep]
    edit (atoms ++ edits ++ removed) i
  ImportTickets ts -> saveTickets ts
  PutTicket t -> saveTickets [t]
  Assign ticket record -> do
    op <- putRow i "Assignment" ticket [("ticket",ticket),("assignee",record)]
    edit [op] i
  Unassign ticket -> do
    xs <- elements i "Assignment"
    if Element ticket `S.member` xs then edit [Delete "Assignment" (Element ticket)] i else pure i
  SetStatus ticket status -> edit [Update "Ticket" (Element ticket) (M.singleton "status" (Element status))] i
  DeleteTicket ticket -> do
    xs <- elements i "Assignment"
    edit ([Delete "Assignment" (Element ticket) | Element ticket `S.member` xs] ++ [Delete "Ticket" (Element ticket)]) i
  Link a b -> do
    let key = T.decodeUtf8 (L.toStrict (encode (S.toAscList (S.fromList [a,b]))))
    op <- putRow i "Link" key [("left",a),("right",b)]
    edit [op] i
  Unlink key -> edit [Delete "Link" (Element key)] i
  where
    saveTickets ts = do
      atoms <- (++) <$> atomEdits i "Text" (map tTitle ts) <*> atomEdits i "Team" (map tTeam ts)
      old <- elements i "Ticket"
      ops <- forM ts $ \t -> do
        status <- if Element (tId t) `S.member` old then field i "Ticket" (tId t) ["status"] else pure "open"
        putRow i "Ticket" (tId t) [("title",tTitle t),("ticketTeam",tTeam t),("status",status)]
      edit (atoms ++ ops) i

-- Actual Delta then Sigma. Declared links and equal normalized emails generate
-- the identity equivalence relation; no application-side union-find exists.
identityGroups :: Instance -> Either Error (M.Map Element [Text])
identityGroups i = do
  c <- schema (SchemaDef ["Roster","Identity","Link"]
    [ArrowDef "email" "Roster" "Identity",ArrowDef "left" "Link" "Roster",ArrowDef "right" "Link" "Roster"] [])
  d <- schema (SchemaDef ["Person"] [] [])
  extraction <- mapping c (instanceSchema i) (MappingDef (M.fromList [(o,o) | o <- objects (definition c)])
                    (M.fromList [(arrow a,Path (source a) [arrow a]) | a <- arrows (definition c)]))
  collapsed <- mapping c d (MappingDef (M.fromList [(o,"Person") | o <- objects (definition c)])
                    (M.fromList [(arrow a,identity "Person") | a <- arrows (definition c)]))
  projected <- delta extraction i
  roots <- elements projected "Roster"
  links <- elements projected "Link"
  reachable <- generated (M.fromList [("Roster",roots),("Link",links)]) projected
  result <- sigma collapsed reachable
  let groups = M.findWithDefault M.empty "Person" (sigmaOrigins result)
  pure (M.map (\origins -> [elementName x | (o,_,x) <- origins, o == "Roster"]) groups)

-- The set of distinct (resolved person, team) memberships is domain assembly.
-- Candidate construction itself is Pi over the incidence diagram below.
candidateRows :: [Roster] -> [Ticket] -> M.Map Element [Text]
  -> Either Error (M.Map Text [(Element,[Text])])
candidateRows rs ts groups = do
  s <- schema (SchemaDef ["Member","Person","Team","Ticket"]
    [ArrowDef "person" "Member" "Person",ArrowDef "team" "Member" "Team",ArrowDef "needs" "Ticket" "Team"] [])
  d <- schema (SchemaDef ["Candidate"] [] [])
  let owner = M.fromList [(key,p) | (p,keys) <- M.toList groups,key <- keys]
      memberships = M.fromListWith (++) [((p,rTeam r),[rId r]) | r <- rs,Just p <- [M.lookup (rId r) owner]]
      memberKey p team = Element (T.decodeUtf8 (L.toStrict (encode [elementName p,team])))
      members = [(memberKey p team,p,Element team,keys) | ((p,team),keys) <- M.toAscList memberships]
      active = filter ((=="open") . tState) ts
      dat = InstanceDef (M.fromList
        [("Member",S.fromList [k | (k,_,_,_) <- members]),("Person",M.keysSet groups),
         ("Team",S.fromList (map (Element . rTeam) rs ++ map (Element . tTeam) active)),
         ("Ticket",S.fromList (map (Element . tId) active))])
        (M.fromList [("person",M.fromList [(k,p) | (k,p,_,_) <- members]),
          ("team",M.fromList [(k,t) | (k,_,t,_) <- members]),
          ("needs",M.fromList [(Element (tId t),Element (tTeam t)) | t <- active])])
  input <- instanceOf s dat
  f <- mapping s d (MappingDef (M.fromList [(o,"Candidate") | o <- objects (definition s)])
          (M.fromList [(arrow a,identity "Candidate") | a <- arrows (definition s)]))
  result <- piMigration f input
  let witnesses = M.elems (M.findWithDefault M.empty "Candidate" (piFamilies result))
      origins = M.fromList [(k,keys) | (k,_,_,keys) <- members]
  pairs <- forM witnesses $ \family -> do
    let get o = maybe (Left (InternalInvariant "missing candidate witness")) Right (M.lookup (o,identity "Candidate") family)
    ticket <- get "Ticket"
    person <- get "Person"
    member <- get "Member"
    pure (elementName ticket,[(person,M.findWithDefault [] member origins)])
  pure (M.fromListWith (++) pairs)

dashboard :: Instance -> Either Error Value
dashboard i = do
  rs <- rosterRows i
  ts <- ticketRows i
  groups <- identityGroups i
  candidates <- candidateRows rs ts groups
  assignments <- elements i "Assignment"
  assigned <- forM (S.toList assignments) $ \(Element key) -> (,) key <$> field i "Assignment" key ["assignee"]
  links <- elements i "Link"
  linkRows <- forM (S.toList links) $ \(Element key) -> do
    a <- field i "Link" key ["left"]
    b <- field i "Link" key ["right"]
    pure (object ["id" .= key,"left" .= a,"right" .= b])
  let byId = M.fromList [(rId r,r) | r <- rs]
      owner = M.fromList [(key,person) | (person,keys) <- M.toList groups,key <- keys]
      nameOf p = case sortOn (\r -> (negate (T.length (rName r)),rId r)) [r | key <- M.findWithDefault [] p groups,Just r <- [M.lookup key byId]] of
        r:_ -> rName r
        [] -> "Unknown"
      rValue r = object ["id" .= rId r,"name" .= rName r,"email" .= rEmail r,"team" .= rTeam r,"source" .= rSource r]
      people = [object ["id" .= p,"name" .= nameOf p,"records" .= [rValue r | key <- keys,Just r <- [M.lookup key byId]],
                        "load" .= length [() | (ticket,key) <- assigned, M.lookup key owner == Just p,
                                      any (\t -> tId t == ticket && tState t == "open") ts]] | (p,keys) <- M.toList groups]
      tickets = [object ["id" .= tId t,"title" .= tTitle t,"team" .= tTeam t,"status" .= tState t,
                  "assignedRecord" .= M.lookup (tId t) (M.fromList assigned),
                  "candidates" .= [object ["person" .= p,"name" .= nameOf p,"records" .= sortOn (\key -> (negate (maybe 0 (T.length . rName) (M.lookup key byId)),key)) keys]
                                  | (p,keys) <- M.findWithDefault [] (tId t) candidates]] | t <- ts]
  pure (object ["people" .= people,"roster" .= map rValue rs,"tickets" .= tickets,"links" .= linkRows,
     "teams" .= S.toAscList (S.fromList (map rTeam rs ++ map tTeam ts)),
     "stats" .= object ["records" .= length rs,"people" .= M.size groups,"requests" .= length ts,
       "open" .= length (filter ((=="open") . tState) ts),"assignments" .= length assigned,
       "candidates" .= sum (map length (M.elems candidates))]])

publicExport :: Instance -> Either Error Value
publicExport i = do
  s <- schema (SchemaDef ["Work","Assignment","Team","Label","State"]
      [ArrowDef "work" "Assignment" "Work",ArrowDef "assigneeName" "Assignment" "Label",
       ArrowDef "team" "Work" "Team",ArrowDef "title" "Work" "Label",ArrowDef "state" "Work" "State"] [])
  f <- mapping s (instanceSchema i) (MappingDef
    (M.fromList [("Work","Ticket"),("Assignment","Assignment"),("Team","Team"),("Label","Text"),("State","Status")])
    (M.fromList [("work",Path "Assignment" ["ticket"]),("assigneeName",Path "Assignment" ["assignee","name"]),
      ("team",Path "Ticket" ["ticketTeam"]),("title",Path "Ticket" ["title"]),("state",Path "Ticket" ["status"])]))
  view <- delta f i
  -- Include open work only. Reachability is essential: Delta alone also pulls
  -- the whole shared Text set, including names of people not in this handoff.
  ts <- ticketRows i
  assignments <- elements view "Assignment"
  let roots = S.fromList [Element (tId t) | t <- ts,tState t == "open"]
  visible <- generated (M.fromList [("Work",roots),("Assignment",assignments `S.intersection` roots)]) view
  table <- rows visible "Work" [("title",Path "Work" ["title"]),("team",Path "Work" ["team"]),("state",Path "Work" ["state"])]
  names <- rows visible "Assignment" [("name",Path "Assignment" ["assigneeName"])]
  let nameMap = M.fromList [(key,M.findWithDefault "" "name" cols) | (key,cols) <- names]
      flat = [object ["id" .= key,"title" .= M.lookup "title" cols,"team" .= M.lookup "team" cols,
                       "status" .= M.lookup "state" cols,"assignee" .= M.lookup key nameMap] | (key,cols) <- table]
  pure (object ["format" .= ("catdb-handoff-v1" :: Text),"rows" .= flat,
                "schema" .= definition s,"instance" .= instanceData visible])

errorMessage :: Error -> Text
errorMessage = \case
  EquationViolation _ _ -> "Assignment rejected: the request and roster entry belong to different teams. Nothing was saved."
  ReferenceViolation _ _ _ -> "A record is still referenced, or the selected record no longer exists. Unassign requests and remove identity links before removing source rows. Nothing was saved."
  QueryLimit name n -> "This workspace exceeds the PoC query budget (" <> name <> ": " <> T.pack (show n) <> ")."
  e -> T.pack (show e)

demoWorkspace :: Either Error Instance
demoWorkspace = do
  i <- workspace
  a <- applyCommand (ImportRoster "Team directory" False
    [Roster "Team directory/1" "Maya Chen" "maya@example.test" "Design" "Team directory",
     Roster "Team directory/2" "Jon Bell" "jon@example.test" "Engineering" "Team directory",
     Roster "Team directory/3" "Iris Park" "iris@example.test" "Support" "Team directory"]) i
  b <- applyCommand (ImportRoster "Contractors" False
    [Roster "Contractors/1" "M. Chen" "maya@example.test" "Design" "Contractors",
     Roster "Contractors/2" "Jon B." "j.bell@example.test" "Engineering" "Contractors",
     Roster "Contractors/3" "Noah Quinn" "noah@example.test" "Design" "Contractors"]) a
  applyCommand (ImportTickets
    [Ticket "REQ-101" "Review the welcome flow" "Design" "open",
     Ticket "REQ-102" "Investigate sync failures" "Engineering" "open",
     Ticket "REQ-103" "Prepare the partner handoff" "Support" "open",
     Ticket "REQ-104" "Illustrate the release notes" "Design" "open",
     Ticket "REQ-105" "Translate the onboarding guide" "Translation" "open"]) b
