module Canvas (schemaCanvas,instanceCanvas,migrationCanvas,schemaInspector,canvasFooter) where

import Data.List (elemIndex)
import Data.Maybe (fromMaybe,listToMaybe,mapMaybe)
import qualified Data.Text as T
import Miso (View,Attribute,text)
import Miso.Event (onWithOptions)
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import qualified Miso.Svg as G
import qualified Miso.Svg.Property as A
import qualified Miso.Event.Types as E
import Miso.Event.Decoder (pointerDecoder)
import Miso.String (ms,fromMisoString)
import DiagramLayout
import Model
import Studio.Wire

type V=View Model Action
type Edge=(S,S,S,S,Action)
divc :: S -> [V] -> V
divc cls=H.div_ [P.class_ cls]
txt :: S -> V
txt=text
button :: S -> S -> Action -> Bool -> V
button cls label action disabled=H.button_ ([P.class_ cls,H.onClick action]++[P.disabled_|disabled]) [txt label]
field :: S -> S -> (S->Action) -> V
field label value action=H.label_ [P.class_ "field"] [txt label,H.input_ [P.value_ value,P.aria_ "label" label,H.onInput action]]
choose :: S -> S -> (S->Action) -> [S] -> V
choose label value action options=H.label_ [P.class_ "field"] [txt label,H.select_ [P.value_ value,H.onChange action,P.aria_ "label" label]
  (H.option_ [P.value_ ""] [txt "選んでください"]:[H.option_ [P.value_ o] [txt o]|o<-options])]
small :: S -> V
small value=H.p_ [P.class_ "hint"] [txt value]
num :: Double -> S
num=ms . show

toolbar :: Model -> [V] -> V
toolbar m controls=divc "canvas-toolbar"
  [divc "mode-tabs" [tab "Schema 構造" Structure [Structure,SchemaEditor],tab "Instance データ" Records [Records],tab "Migration 移行" Mappings [Mappings,MappingEditor,ViewBuilder]]
  ,divc "canvas-actions" [button "tool-btn" "−" (ZoomDiagram 0.85) False,txt (ms (round (diagramZoom m*100)::Int)<>"%")
    ,button "tool-btn" "＋" (ZoomDiagram 1.18) False,button "fit-btn" "全体表示 ↗" FitDiagram False]
  ,if null controls then H.span_ [] [] else divc "diagram-tools" controls]
  where tab label target pages=button ("mode-tab"<>if page m `elem` pages then " selected" else "") label (Navigate target) False

frame :: Model -> S -> S -> [V] -> Double -> Double -> [V] -> V -> V
frame m label meta controls width height content annotation=divc ("canvas-workspace"<>if page m `elem` [Mappings,MappingEditor,ViewBuilder] then " migration-workspace" else "")
  [divc "page-head" [divc "eyebrow" [txt label],H.h1_ [] [txt title],H.p_ [] [txt description]]
  ,divc "canvas-panel" [toolbar m controls,divc "diagram-viewport"
    [divc "viewport-label" [txt ("✳  "<>label),H.span_ [] [txt meta]]
    ,G.svg_ [P.class_ "graph-svg",A.viewBox_ ("0 0 "<>num width<>" "<>num height),P.role_ "group",P.aria_ "label" label
      ,H.onPointerDown (\e->let (x,y)=E.client e in BeginPan x y)
      ,H.onPointerMove (\e->let (x,y)=E.client e in MovePointer x y),H.onPointerUp (const EndGesture),H.onPointerLeave (const EndGesture)]
      [markers,G.g_ [A.transform_ ("translate("<>num px<>" "<>num py<>") translate("<>num (width/2)<>" "<>num (height/2)<>") scale("<>num (diagramZoom m)<>") translate("<>num (-width/2)<>" "<>num (-height/2)<>")")] content]
    ,annotation]
  ,divc "canvas-bottom" [txt "ドラッグで移動 · 背景をドラッグでパン · Tab / Enterで選択",divc "pan-controls"
      [button "small-link" "←" (PanDiagram 60 0) False,button "small-link" "↑" (PanDiagram 0 60) False
      ,button "small-link" "↓" (PanDiagram 0 (-60)) False,button "small-link" "→" (PanDiagram (-60) 0) False
      ,button "small-link" "再配置" RearrangeDiagram False]]]]
  where
    Point px py=diagramPan m
    (title,description)=case page m of
      Records->("関係を、たどって確かめる。",if editingAllowed m then "選んだ一行から続く矢印と行き先。変更は下書きとして図に現れます。" else "選んだ一行から続く矢印と行き先を、計算に使った版の由来とともにたどります。")
      MappingEditor->("一つの対応、三つの計算。","対象は対象へ、生成射は道へ。構造を保つ対応を描きます。")
      Mappings->("一つの対応、三つの計算。","対応を選んで、計算の向きと道の像を見渡します。")
      ViewBuilder->("一つの対応、三つの計算。","確定した入力版から計算し、結果の由来をたどります。")
      _->("構造を、そのまま描く。","対象と矢印をつなぎ、二つの道が等しいことまで定義する。")

markers :: V
markers=G.defs_ [] [G.marker_ [P.id_ ("arr-"<>tone),A.markerWidth_ "10",A.markerHeight_ "10",A.refX_ "8",A.refY_ "5",A.orient_ "auto"]
  [G.path_ [A.d_ "M1 1L8 5L1 9",A.fill_ "none",A.stroke_ color,A.strokeWidth_ "1.5"]]
  |(tone,color)<-[("base","#7c9c8a"),("green","#398b70"),("amber","#c79443"),("bad","#be5345"),("violet","#7767a8")]]

clickable :: S -> Action -> [MisoAttribute]
clickable label action=[P.role_ "button",P.tabindex_ "0",P.aria_ "label" label,H.onClick action
  ,H.onKeyDown (\key->if key==13 || key==32 then action else Noop)]

type MisoAttribute = Attribute Action

node :: Model -> S -> Point -> S -> S -> S -> S -> Action -> V
node m key point@(Point x y) title subtitle secondary tone action=G.g_
  ([P.class_ ("graph-node "<>tone),A.transform_ ("translate("<>num x<>" "<>num y<>")")]
   ++clickable (title<>" · "<>subtitle<>(if page m==Records then " · "<>secondary else "")) action
   ++[onWithOptions E.BUBBLE E.stopPropagation "pointerdown" pointerDecoder (\e _->let (cx,cy)=E.client e in BeginMove key point cx cy)])
  [G.rect_ [P.width_ "198",P.height_ "110",A.rx_ "15"]
  ,G.text_ [P.class_ "node-type",A.x_ "20",A.y_ "27"] [txt subtitle]
  ,G.text_ [P.class_ "node-title",A.x_ "20",A.y_ "64"] [txt (short (if page m==Records then 16 else 11) title)]
  ,G.text_ [P.class_ "node-count",A.x_ "20",A.y_ "90"] [txt (short 26 secondary)]
  ,G.circle_ [P.class_ "port",A.cx_ "198",A.cy_ "55",A.r_ "5"]
  ,G.title_ [] [txt (title<>" · "<>secondary)]]
  where
    short :: Int -> S -> S
    short n=ms . T.pack . fit n . T.unpack . fromMisoString
    fit :: Int -> String -> String
    fit _ []=[]
    fit remaining (c:rest) | cost>remaining="…"
                           | otherwise=c:fit (remaining-cost) rest
      where cost=if c>='\x2e80' then 2 else 1

drawEdges :: [(S,Point)] -> [Edge] -> [V]
drawEdges=drawEdgesWith routeEdge

drawEdgesWith :: (Point -> Point -> Int -> Int -> Route) -> [(S,Point)] -> [Edge] -> [V]
drawEdgesWith routeFor positions edges=mapMaybe draw (zip [0::Int ..] edges)
  where
    draw (index,(key,source,target,tone,action))=do
      a<-lookup source positions; b<-lookup target positions
      let siblings=[i | (i,(_,s,t,_,_))<-zip [0::Int ..] edges,s==source,t==target]
          lane=fromMaybe 0 (elemIndex index siblings)
          route=routeFor a b lane (length siblings)
          Point lx ly=routeLabel route
          pillWidth=max 62 (min 220 (fromIntegral (T.length (fromMisoString key))*8+28))
          coords (Point x y)=num x<>" "<>num y
          path="M"<>coords (routeStart route)<>" C"<>coords (routeControlA route)<>" "<>coords (routeControlB route)<>" "<>coords (routeEnd route)
      pure (G.g_ ([P.class_ ("graph-edge "<>tone)]++clickable (key<>" : "<>source<>" → "<>target) action)
        [G.path_ [P.class_ "edge-hit",A.d_ path],G.path_ [P.class_ "edge-line",A.d_ path,A.markerEnd_ ("url(#arr-"<>tone<>")")]
        ,G.rect_ [A.x_ (num (lx-pillWidth/2)),A.y_ (num (ly-17)),P.width_ (num pillWidth),P.height_ "32",A.rx_ "16"]
        ,G.text_ [A.x_ (num lx),A.y_ (num (ly+4)),A.textAnchor_ "middle"] [txt key]])

pinned :: Model -> [(S,Point)]
pinned m=[(ms (T.drop (T.length prefix) (fromMisoString k)),p) | (k,p)<-diagramPositions m,prefix `T.isPrefixOf` fromMisoString k]
  where prefix=fromMisoString (diagramScope m)

schemaCanvas :: Model -> V
schemaCanvas m=frame m ("SCHEMA / "<>if page m==SchemaEditor then "NEW DEFINITION" else browsedSchema m)
  (if page m==SchemaEditor then if schemaChecked m then "VALIDATED · UNSAVED" else "DRAFT" else "COMMITTED · FINITE / ACYCLIC") controls (layoutWidth layout) (layoutHeight layout)
  (drawEdges positions edges++[node m o p o (if page m==SchemaEditor then "DRAFT OBJECT" else "SCHEMA / OBJECT")
    (if page m==SchemaEditor then "未保存の対象" else if schema==currentSchema m then ms (length (records (visibleData m) o))<>" records" else "スキーマの対象")
    (if diagramSelection m==Just (SelectObject o) then "selected" else "") (DiagramObject False o) | Placed o p<-layoutNodes layout]) equationStrip
  where
    schema=displayedSchema m
    layout=layoutDag (schemaObjects schema) [(arrowFrom a,arrowTo a)|a<-schemaArrows schema] (pinned m)
    positions=[(o,p)|Placed o p<-layoutNodes layout]
    selectedEquation=case diagramSelection m of Just (SelectEquation i)->listToMaybe (drop i (schemaEquations schema)); _->Nothing
    leftSteps=if page m==SchemaEditor then equationLeftInput m else maybe [] (pathSteps . equationLeft) selectedEquation
    rightSteps=if page m==SchemaEditor then equationRightInput m else maybe [] (pathSteps . equationRight) selectedEquation
    tone key | key `elem` leftSteps="green" | key `elem` rightSteps="amber" | otherwise="base"
    edges=[(arrowId a,arrowFrom a,arrowTo a,tone (arrowId a),DiagramArrow False (arrowId a)) | a<-schemaArrows schema]
    controls=if page m==SchemaEditor then
      [button "btn small" "対象を追加" (SetDiagramTool InspectTool) False
      ,button ("btn small"<>if isConnect then " chosen" else "") "射をつなぐ" (SetDiagramTool (ConnectTool Nothing)) False
      ,button "btn small" "経路 A を選ぶ" (SetDiagramTool (EquationTool True)) False
      ,button "btn small" "経路 B を選ぶ" (SetDiagramTool (EquationTool False)) False]
      else [button "btn small" "別名で新しく作る" (StartSchema True) False,button "btn small" "空のスキーマ" (StartSchema False) False]
    isConnect=case diagramTool m of ConnectTool _->True; _->False
    equationStrip=divc "floating-bottom" [divc "equation-header" [txt "PATH EQUATION · 左から右へ合成"]
      ,case selectedEquation of
        Just eq->divc "equation-content" [H.code_ [P.class_ "green-text"] [txt (pathLabel (equationLeft eq))],txt "≡",H.code_ [P.class_ "amber-text"] [txt (pathLabel (equationRight eq))]]
        Nothing->small (if null (schemaObjects schema) then "対象を追加して、図を描き始めます。" else if page m==SchemaEditor then "対象で始点を選び、矢印を順に選んで二つの道を定義。" else "右側で対象・生成射・宣言された等式を選べます。")]

schemaInspector :: Model -> V
schemaInspector m=divc "schema-inspector"
  [divc "inspector-kicker" [txt "INSPECTOR / SCHEMA"],H.h2_ [] [txt (if page m==SchemaEditor then "構造を描く" else "図式の約束")]
  ,if page m==SchemaEditor then editor else choose "スキーマ" (browsedSchema m) BrowseSchema (map fst (maybe [] stateSchemas (snapshotState m)))
  ,divc "object-chips" [button ("chip"<>if diagramSelection m==Just (SelectObject o) then " chosen" else "") o (SelectDiagram (SelectObject o)) False|o<-schemaObjects schema]
  ,selectionDetails
  ,H.h3_ [] [txt "宣言された経路等式"]
  ,if null (schemaEquations schema) then small "等式の宣言はありません。" else divc "equation-list"
    [button ("equation-choice"<>if diagramSelection m==Just (SelectEquation i) then " chosen" else "") (pathLabel (equationLeft eq)<>" ≡ "<>pathLabel (equationRight eq)) (SelectDiagram (SelectEquation i)) False|(i,eq)<-zip [0..] (schemaEquations schema)]
  ,if page m==SchemaEditor then divc "schema-save" [button "btn" "定義を検査" CheckSchema (isPending m||blank (schemaNameInput m))
      ,small (if schemaChecked m then "Validated · 検査済み、まだ保存していません。" else "Draft · 保存前にエンジンで定義を検査します。")
      ,button "btn primary" "新しいスキーマとして保存" (Guard SaveSchema) (isPending m||not (schemaChecked m))]
    else small "Committed · 定義は不変です。変更するときは別名の新しいスキーマを作ります。"]
  where
    schema=displayedSchema m
    editor=divc "schema-edit-controls"
      [field "新しいスキーマ名" (schemaNameInput m) (Edit SchemaNameField)
      ,field "追加する対象名" (newObjectInput m) (Edit NewObjectField),button "btn" "＋ 対象を追加" AddObject False
      ,small (case diagramTool m of ConnectTool Nothing->"図で始点、終点の順に対象を選んでください。"; ConnectTool (Just o)->o<>" から接続する終点を選んでください。"; EquationTool side->(if side then "経路 A" else "経路 B")<>"：共通の始点を選び、矢印を順に選びます。"; _->"射をつなぐボタンから、二つの対象を選べます。")
      ,field "生成射の名前" (arrowNameInput m) (Edit ArrowNameField)
      ,choose "始点" (arrowFromInput m) (Edit ArrowFromField) (schemaObjects schema)
      ,choose "終点" (arrowToInput m) (Edit ArrowToField) (schemaObjects schema),button "btn" "＋ 射を追加" AddArrow False
      ,choose "二経路の共通の始点" (equationStartInput m) (Edit EquationStartField) (schemaObjects schema)
      ,pathEntry True (equationLeftInput m),pathEntry False (equationRightInput m)
      ,button "btn" "二つの道を等式として追加" AddEquation (blank (equationStartInput m))]
    pathEntry side steps=divc (if side then "path-entry green" else "path-entry amber")
      [button "small-link" (if side then "経路 A を図で選ぶ" else "経路 B を図で選ぶ") (SetDiagramTool (EquationTool side)) False
      ,H.code_ [] [txt (pathLabel (WPath (equationStartInput m) steps))]
      ,divc "nexts" [button "stepbtn" (arrowId a) (AppendEquation side (arrowId a)) False | a<-maybe [] (outgoing schema) (pathEndObject schema (equationStartInput m) steps)]
      ,button "small-link" "リセット / 恒等の道" (ClearEquation side) False]
    selectionDetails=case diagramSelection m of
      Just (SelectObject o)->divc "selection-details"
        [H.h3_ [] [txt o],small ("出る射："<>join [arrowId a|a<-schemaArrows schema,arrowFrom a==o])
        ,small ("入る射："<>join [arrowId a|a<-schemaArrows schema,arrowTo a==o])
        ,divc "nexts" [button "small-link" "←" (NudgeNode (-25) 0) False,button "small-link" "↑" (NudgeNode 0 (-25)) False,button "small-link" "↓" (NudgeNode 0 25) False,button "small-link" "→" (NudgeNode 25 0) False]
        ,if page m==SchemaEditor then button "small-link danger" "この対象を削除" (RemoveObject o) False else H.span_ [] []]
      Just (SelectArrow key)->case arrowDef schema key of
        Just a->divc "selection-details" [H.h3_ [] [txt (arrowId a)],small (arrowFrom a<>" → "<>arrowTo a),if page m==SchemaEditor then button "small-link danger" "この射を削除" (RemoveArrow key) False else H.span_ [] []]
        Nothing->small "選んだ射がありません。"
      Just (SelectEquation i)->case listToMaybe (drop i (schemaEquations schema)) of
        Just eq->divc "selection-details" [H.h3_ [] [txt "同じ始点、同じ終点。"],pathLine "PATH A" "green-text" (equationLeft eq),pathLine "PATH B" "amber-text" (equationRight eq)
          ,small "これはスキーマに宣言された等式です。すべての始点の行で両経路の到達先が一致することを要求します。"
          ,if page m==SchemaEditor then button "small-link danger" "この等式を削除" (RemoveEquation i) False else H.span_ [] []]
        Nothing->small "対象、生成射、経路等式を選んで詳細を表示します。"
      _->H.span_ [] []
    pathLine label cls path=divc ("path-code "<>cls) [small label,H.code_ [P.class_ cls] [txt (pathStart path<>" → "<>pathLabel path<>" → "<>fromMaybe "?" (pathEndObject schema (pathStart path) (pathSteps path)))]]
    join []="—"; join xs=ms (T.intercalate ", " (map fromMisoString xs))

instanceCanvas :: Model -> V
instanceCanvas m=frame m ("INSTANCE LENS / "<>focusedObject m<>maybe "" (" · "<>) (focusedRow m))
  (if committedVisible m then "COMMITTED" else if verified m then "VALIDATED · UNSAVED" else if dirty m then "DRAFT · UNSAVED" else "COMMITTED") [] (layoutWidth layout) (layoutHeight layout)
  (drawEdges positions edges++[node m key p (caption m object row) (object<>" / ELEMENT") ("ID · "<>row) (if (object,row)==focus then "selected" else if key `elem` badEnds then "invalid" else "") (FocusRow object row)
    | Placed key p<-layoutNodes layout,Just (object,row)<-[lookup key keyed]]) strip
  where
    schema=currentSchema m; dat=visibleData m; focus=(focusedObject m,fromMaybe "" (focusedRow m))
    allNodes=closure 32 [] [focus]
    closure limit seen []=take limit seen
    closure limit seen (n@(o,x):rest) | n `elem` seen=closure limit seen rest
      | length seen>=limit=seen
      | otherwise=closure limit (seen++[n]) (rest++[(arrowTo a,y)|a<-outgoing schema o,Just y<-[fieldTarget dat o x (arrowId a)]])
    keyOf (o,x)=o<>" / "<>x
    keyed=[(keyOf n,n)|n<-allNodes,not (blank (snd n))]
    selectedEq=listToMaybe [eq|eq<-schemaEquations schema,pathStart (equationLeft eq)==fst focus]
    result=fmap (\eq->equationCheck schema dat eq (snd focus)) selectedEq
    badEnds=case result of Just (Right a,Right b,False)->map keyOf (mapMaybe lastNode [a,b]); _->[]
    routePairs path=case follow schema dat path (snd focus) of Right ns->zip3 (pathSteps path) ns (drop 1 ns); _->[]
    left=maybe [] (routePairs . equationLeft) selectedEq
    right=maybe [] (routePairs . equationRight) selectedEq
    tone triple | triple `elem` left="green" | triple `elem` right="amber" | otherwise="base"
    edges=[(arrowId a,keyOf n,keyOf end,tone (arrowId a,n,end),if editingAllowed m then Pick o x (arrowId a) else FocusRow (fst end) (snd end))
      | n@(o,x)<-allNodes,a<-outgoing schema o,Just y<-[fieldTarget dat o x (arrowId a)],let end=(arrowTo a,y),end `elem` allNodes]
    layout=layoutDag (map fst keyed) [(a,b)|(_,a,b,_,_)<-edges] (pinned m)
    positions=[(o,p)|Placed o p<-layoutNodes layout]
    strip=divc ("floating-bottom"<>if null badEnds then "" else " invalid")
      [divc "equation-header" [txt (if selectedEq==Nothing then "FOCUSED NEIGHBORHOOD / 選んだ行からたどる" else "PATH VALIDATION / 表示中の行からたどる")]
      ,case result of
        Just (a,b,ok)->divc "equation-content" [txt (if ok then "✓ 二つの道は "<>endpoint a<>" で一致" else "≠ 経路 A："<>endpoint a<>" / 経路 B："<>endpoint b)]
        Nothing->small (if null keyed then "この対象には行がありません。表から行を追加できます。" else "選んだ一行と、その行から到達する近傍を表示しています。")
      ,if length allNodes>=32 then small "近傍の先頭32要素を表示。行を選ぶと、その位置を中心に展開します。"
        else small (if editingAllowed m then "線のラベルを選ぶと行き先を変更できます。下書き全体は上部の「下書きを確認」で検査します。" else "読み取り専用の版です。線のラベルから行き先へ進み、右側から元の版の由来を開けます。")]
    endpoint=either id (maybe "行き先なし" (\(o,x)->o<>" / "<>x) . lastNode)

migrationCanvas :: Model -> V
migrationCanvas m=frame m ("MIGRATION / F : "<>mappingFrom f<>" → "<>mappingTo f)
  (if page m==MappingEditor then if mappingChecked m then "VALIDATED · UNSAVED" else "DRAFT" else "COMMITTED MAPPING") controls width height content strip
  where
    f=mappingDraft m
    source=fromMaybe (WSchema [] [] []) (mappingInputSchema m)
    target=fromMaybe (WSchema [] [] []) (mappingOutputSchema m)
    left=layoutVertical (schemaObjects source) [(arrowFrom a,arrowTo a)|a<-schemaArrows source] []
    right=layoutVertical (schemaObjects target) [(arrowFrom a,arrowTo a)|a<-schemaArrows target] []
    shift=layoutWidth left+85; width=shift+layoutWidth right; height=max (layoutHeight left) (layoutHeight right)
    prefix side key=side<>key
    automatic=[(prefix "C:" o,p)|Placed o p<-layoutNodes left]++[(prefix "D:" o,Point (pointX p+shift) (pointY p))|Placed o p<-layoutNodes right]
    positions=[(key,fromMaybe p (lookup key (pinned m)))|(key,p)<-automatic]
    pathObjects=either (const []) id (pathNodes [(arrowId a,arrowFrom a,arrowTo a)|a<-schemaArrows target] (pathStart (mappingSelectedPath m)) (pathSteps (mappingSelectedPath m)))
    edges side schema=[(arrowId a,prefix side (arrowFrom a),prefix side (arrowTo a),if side=="D:" && arrowId a `elem` pathSteps (mappingSelectedPath m) || side=="C:" && arrowId a==selectedMappingArrow m then "violet" else "base",DiagramArrow (side=="D:") (arrowId a))|a<-schemaArrows schema]
    content=[G.text_ [P.class_ "schema-boundary-title",A.x_ "70",A.y_ "55"] [txt ("C / "<>mappingFrom f)]
      ,G.text_ [P.class_ "schema-boundary-title",A.x_ (num (shift+70)),A.y_ "55"] [txt ("D / "<>mappingTo f)]]
      ++[mappingLine o image| (o,image)<-mappingObjects f]
      ++drawEdgesWith routeVerticalEdge positions (edges "C:" source++edges "D:" target)
      ++[node m key p o (if "C:" `T.isPrefixOf` fromMisoString key then "C / OBJECT" else "D / OBJECT") "SCHEMA OBJECT" (if "D:" `T.isPrefixOf` fromMisoString key && o `elem` pathObjects then "mapped" else "") (DiagramObject ("D:" `T.isPrefixOf` fromMisoString key) o)
        |(key,p)<-positions,let o=ms (T.drop 2 (fromMisoString key))]
    mappingLine o image=case (lookup (prefix "C:" o) positions,lookup (prefix "D:" image) positions) of
      (Just (Point x y),Just (Point tx ty))->G.path_ [P.class_ "mapping-line",A.d_ ("M"<>num (x+198)<>" "<>num (y+55)<>" C"<>num (shift-30)<>" "<>num (y+55)<>" "<>num (shift-30)<>" "<>num (ty+55)<>" "<>num tx<>" "<>num (ty+55)),A.markerEnd_ "url(#arr-violet)"]
      _->H.span_ [] []
    controls=if page m==MappingEditor then [button "btn small" "対象を対応させる" (SetDiagramTool (ObjectMapTool Nothing)) False] else []
    strip=divc "floating-bottom migration-strip" [divc "equation-header" [txt (if operationInput m=="delta" then "Δ_F : D-INSTANCE → C-INSTANCE" else if operationInput m=="sigma" then "Σ_F : C-INSTANCE → D-INSTANCE" else "Π_F : C-INSTANCE → D-INSTANCE")]
      ,small (if blank (selectedMappingArrow m) then "C の射を選び、D の矢印を順に選ぶと、生成射を道へ写せます。" else selectedMappingArrow m<>" ↦ "<>pathLabel (mappingSelectedPath m))
      ,if page m==ViewBuilder then small ("入力する確定版："<>if blank (viewSourceInput m) then "未選択" else viewSourceInput m) else H.span_ [] []]

canvasFooter :: Model -> V
canvasFooter m=divc "bottom-status" [H.span_ [] [txt (if isPending m then "● COMPUTING" else if page m==SchemaEditor then if schemaChecked m then "● VALIDATED · 検証済み・未確定" else "● DRAFT · 未検証" else if page m==MappingEditor then if mappingChecked m then "● VALIDATED · 検証済み・未確定" else "● DRAFT · 未検証" else if dirty m then if verified m then "● VALIDATED · 未保存" else "● DRAFT · 未保存" else "● COMMITTED")]
  ,H.span_ [] [txt "FINITE CATEGORY · 1 FILE · LOCAL"]]
