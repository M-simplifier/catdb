module Demo (todoInstance, publicView, spanExample, addTask) where

import CatDB
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Text (Text)

todoInstance :: Either Error Instance
todoInstance = do
  s <- schema (SchemaDef ["Task","Person","Project","Text","State"]
    [ ArrowDef "project" "Task" "Project"
    , ArrowDef "owner" "Task" "Person"
    , ArrowDef "personProject" "Person" "Project"
    , ArrowDef "title" "Task" "Text"
    , ArrowDef "status" "Task" "State"
    , ArrowDef "projectName" "Project" "Text"
    , ArrowDef "personName" "Person" "Text" ]
    [Equation (Path "Task" ["project"]) (Path "Task" ["owner","personProject"])])
  edit [ Insert "Text" "Personal" M.empty, Insert "Text" "Work" M.empty
       , Insert "Text" "Alice" M.empty, Insert "Text" "Bob" M.empty
       , Insert "Text" "Try a categorical database" M.empty
       , Insert "State" "open" M.empty, Insert "State" "done" M.empty
       , Insert "Project" "home" (M.singleton "projectName" "Personal")
       , Insert "Project" "work" (M.singleton "projectName" "Work")
       , Insert "Person" "alice" (M.fromList [("personProject","home"),("personName","Alice")])
       , Insert "Person" "bob" (M.fromList [("personProject","work"),("personName","Bob")])
       , Insert "Task" "first" (M.fromList [("project","home"),("owner","alice"),
                       ("title","Try a categorical database"),("status","open")]) ] (empty s)

addTask :: Element -> Element -> Element -> Text -> Instance -> Either Error Instance
addTask key project owner title i = do
  texts <- elements i "Text"
  let t = Element title
      newText = [Insert "Text" t M.empty | t `S.notMember` texts]
  edit (newText ++ [Insert "Task" key (M.fromList
    [("project",project),("owner",owner),("title",t),("status","open")])]) i

-- One schema map defines a public view, including a composed path.
publicView :: Schema -> Either Error Mapping
publicView real = do
  view <- schema (SchemaDef ["Card","Text"]
    [ArrowDef "caption" "Card" "Text",ArrowDef "assignee" "Card" "Text"] [])
  mapping view real (MappingDef (M.fromList [("Card","Task"),("Text","Text")])
    (M.fromList [("caption",Path "Task" ["title"]),
                 ("assignee",Path "Task" ["owner","personName"])]))

-- A span Person -> Department <- Ticket. The SAME schema mapping to one
-- object computes a union modulo department links (Sigma) or an equijoin (Pi).
spanExample :: Either Error (Mapping,Instance)
spanExample = do
  s <- schema (SchemaDef ["Person","Department","Ticket"]
    [ArrowDef "personDept" "Person" "Department",ArrowDef "ticketDept" "Ticket" "Department"] [])
  t <- schema (SchemaDef ["Result"] [] [])
  i <- edit
    [ Insert "Department" "engineering" M.empty, Insert "Department" "design" M.empty
    , Insert "Person" "alice" (M.singleton "personDept" "engineering")
    , Insert "Person" "bob" (M.singleton "personDept" "design")
    , Insert "Ticket" "T1" (M.singleton "ticketDept" "engineering")
    , Insert "Ticket" "T2" (M.singleton "ticketDept" "engineering")
    , Insert "Ticket" "T3" (M.singleton "ticketDept" "design") ] (empty s)
  f <- mapping s t (MappingDef (M.fromList [(o,"Result") | o <- objects (definition s)])
       (M.fromList [(arrow a,identity "Result") | a <- arrows (definition s)]))
  pure (f,i)
