module View (viewModel) where

import Data.List (find)
import Data.Maybe (fromMaybe,isJust,listToMaybe)
import qualified Data.Text as T
import Miso (View,text)
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import Miso.String (ms,fromMisoString)
import Model
import Studio.Wire
import Course (coursePage,coursePanel,courseRail,courseOrigins)

type V = View Model Action
empty :: V
empty = H.span_ [] []
txt :: S -> V
txt = text
divc :: S -> [V] -> V
divc cls = H.div_ [P.class_ cls]
p :: S -> V
p value = H.p_ [] [txt value]
hint :: S -> V
hint value = H.p_ [P.class_ "hint"] [txt value]
button :: S -> S -> Action -> Bool -> V
button cls label action disabled = H.button_ ([P.class_ cls,H.onClick action]++[P.disabled_ | disabled]) [txt label]
btn :: S -> Action -> V
btn label action = button "btn" label action False
input :: S -> S -> (S -> Action) -> V
input label value change = H.input_ [P.aria_ "label" label,P.placeholder_ label,P.value_ value,H.onInput change]
field :: S -> V -> V
field label control = H.label_ [P.class_ "field"] [H.span_ [] [txt label],control]
select :: S -> S -> (S -> Action) -> [(S,S)] -> V
select label value action choices = H.select_ [P.aria_ "label" label,P.value_ value,H.onChange action]
  (H.option_ [P.value_ ""] [txt "選んでください"]:[H.option_ [P.value_ key] [txt name] | (key,name) <- choices])
heading :: S -> S -> S -> V
heading kick title lead = divc "ph" [divc "kick" [txt kick],H.h1_ [] [txt title],H.p_ [P.class_ "lead"] [txt lead]]
mathNote :: Model -> S -> V
mathNote m value = if math m then divc "mathbox" [txt value] else empty

viewModel :: props -> Model -> V
viewModel _ m = divc ("app"<>if math m then " math" else "")
  [topbar m,rail m,H.main_ [P.class_ "main"] [feedbackView m,busyView m,if page m/=Course then coursePanel m else empty,mainView m],H.aside_ [P.class_ "insp",P.aria_ "label" "選んだ行の道と由来"] [courseOrigins m,inspector m],confirmView m]

topbar :: Model -> V
topbar m = H.header_ [P.class_ "top"]
  [H.button_ [P.class_ "brand",H.onClick (Navigate Files)] [txt "CatDB Studio",H.span_ [P.class_ "version"] [txt "PoC 0.3"]]
  ,H.button_ [P.class_ "dbname",H.onClick (Navigate Files),P.title_ (fromMaybe "ファイルを開く" (snapshotState m >>= statePath))]
    [txt (maybe "ファイルを開く" shortPath (snapshotState m >>= statePath))]
  ,divc "mid"
    [divc "rev" [H.span_ [P.class_ "dot"] [],txt (maybe "未接続" (("確定 "<>) . snapshotRevision) (snapshot m))]
    ,if dirty m then divc "seg"
      [H.button_ [P.aria_ "pressed" (if committedVisible m then "false" else "true"),H.onClick (ShowCommitted False)] [txt "下書き"]
      ,H.button_ [P.aria_ "pressed" (if committedVisible m then "true" else "false"),H.onClick (ShowCommitted True)] [txt "確定版"]] else empty
    ,if dirty m then button "btn" "破棄" DiscardDraft (isPending m) else empty
    ,if dirty m then button "btn" "下書きを確認" PreviewDraft (isPending m) else empty
    ,if dirty m then button "btn primary" "まとめて適用" ApplyDraft (isPending m || not (verified m)) else empty
    ]]
  where shortPath = ms . lastPart . T.replace "\\" "/" . fromMisoString
        lastPart value = case reverse (T.splitOn "/" value) of x:_ -> x; [] -> value

rail :: Model -> V
rail m | isJust (fieldCourse m) && not (guideTools m)=courseRail m
       | otherwise=fullRail m
fullRail :: Model -> V
fullRail m = H.nav_ [P.class_ "rail",P.aria_ "label" "データベース"]
  ([if isJust (fieldCourse m) then button "ri on" "現場サービスの体験へ" (Navigate Course) False else empty
   ,if isJust (fieldCourse m) then button "ri" "体験用の道具に戻す" ToggleGuideTools False else empty
   ,divc "rh" [txt "編集するデータ"]]
  ++ [button ("ri"<>if snapshotRevisionOf m==rev && selectedView m==Nothing then " on" else "") (name<>"  ·  "<>rev)
        (Guard (OpenRevision rev Nothing Nothing)) (isPending m) | (name,rev) <- maybe [] stateHeads (snapshotState m)]
  ++ [divc "rh" [txt (if maybe False snapshotDerived (snapshot m) then "派生したレコード" else "レコード")]]
  ++ [H.button_ [P.class_ ("ri"<>if page m==Records && focusedObject m==o then " on" else ""),H.onClick (FocusObject o)]
        [txt o,H.span_ [P.class_ "n"] [txt (ms (length (records (visibleData m) o)))]] | o <- schemaObjects (currentSchema m)]
  ++ [nav Rules "二つの道の約束",nav Structure "スキーマ",button "ri add" "＋ スキーマを作る" (Navigate SchemaEditor) (isPending m)
     ,divc "rh" [txt "再利用するビュー"]]
  ++ [H.button_ ([P.class_ ("ri view"<>if selectedView m==Just (viewKey v) then " on" else ""),H.onClick (Guard (OpenRevision (viewOutput v) Nothing (Just (viewKey v))))]++[P.disabled_ | isPending m])
      [H.span_ [P.class_ "dvm"] [],txt (viewKey v),H.span_ [P.class_ ("n"<>if viewFreshness v=="stale" then " stale" else "")]
        [txt (if viewFreshness v=="stale" then "古い版" else viewInput v)]] | v <- maybe [] stateViews (snapshotState m)]
  ++ [nav ViewBuilder "＋ ビューを作る",nav Mappings "保存した対応",nav History "版の履歴"
     ,divc "railfoot"
       [H.button_ [P.class_ "switch",P.role_ "switch",P.aria_ "checked" (if math m then "true" else "false"),H.onClick ToggleMath]
          [H.span_ [P.class_ "tr"] [],txt "数学の名前を表示"]
       ,nav Files "ファイルと書き出し",H.p_ [P.class_ "disc"] [txt "この端末の .catdb に保存。確定した版は履歴に残ります。"]]])
  where nav target label = button ("ri"<>if page m==target then " on" else "") label (Navigate target) False

snapshotRevisionOf :: Model -> S
snapshotRevisionOf = maybe "" snapshotRevision . snapshot

feedbackView :: Model -> V
feedbackView m = case feedback m of
  Nothing -> empty
  Just message -> divc ("banner"<>if failure m/=Nothing then " bad" else " info")
    [H.span_ [P.role_ "status",P.aria_ "live" "polite"] [txt message]
    ,maybe empty (\err -> if blank (problemDetail err) then empty else H.details_ [] [H.summary_ [] [txt "詳細"],H.pre_ [] [txt (problemDetail err)]]) (failure m)
    ,button "btn small" "閉じる" Dismiss False]
busyView :: Model -> V
busyView m = if isPending m then divc "busy" [H.span_ [P.role_ "status"] [txt "処理しています…"],
  if cancellable then button "btn small" "計算を取り消す" CancelWork False else empty] else empty
  where cancellable=case pending m of Just (Inspecting _ _ _) -> False; Just ChangingFile -> False; Just Other -> False; _ -> True

mainView :: Model -> V
mainView m
  | blank (token m) = divc "welcome" [heading "この端末で起動" "CatDB Studioを開く" "起動時のウィンドウに表示されたURLから開いてください。URLには、この起動だけに使う接続情報が含まれます。"]
  | otherwise = case page m of
    Course -> coursePage m
    Files -> filesPage m
    SchemaEditor -> schemaEditor m
    MappingEditor -> mappingEditor m
    ViewBuilder -> viewBuilder m
    Mappings -> mappingsPage m
    History -> historyPage m
    Structure -> structurePage m
    Rules -> rulesPage m
    Records -> recordsPage m

filesPage :: Model -> V
filesPage m = divc "page-stack"
  [heading "LOCAL DATABASE" "つながりを、確かめながら編集する。" "レコードを選び、矢印をたどり、二つの道が同じ行に着くか確かめます。変更は下書きでまとめ、整ったところで一つの版として保存します。"
  ,maybe empty (\path -> divc "stepc" [H.h2_ [] [txt "開いているファイル"],H.p_ [P.class_ "file-path"] [txt path],btn "ファイルを閉じる" (Guard CloseDatabase)]) (snapshotState m >>= statePath)
  ,divc "stepc"
    [H.h2_ [] [txt "新しく作る・開く"],field "ファイル名、またはフルパス" (input "例：notes.catdb または C:\\Users\\…\\notes.catdb" (fileInput m) (Edit FilePathField))
    ,hint "ファイル名だけなら、起動ウィンドウに表示されたdataフォルダーに保存します。既存のファイルは上書きしません。"
    ,field "新しく作るときの中身" (select "中身" (presetInput m) (Edit PresetField)
      [("field-service","現場サービス — 原本をそろえ、派遣候補を計算する"),("reading","読書メモ — 二つの道と Δ の見本"),("families","対応の見本 — Σ の同一視と Π の組"),("blank","空のデータベース — 自分のスキーマを作る")])
    ,divc "form-actions" [button "btn primary" "新しく作る" (Guard CreateDatabase) (isPending m),button "btn" "既存ファイルを開く" (Guard OpenDatabase) (isPending m)]]
  ,case snapshotState m >>= stateLegacy of
    Nothing -> empty
    Just _ -> divc "stepc legacy"
      [H.h2_ [] [txt "既存データを取り込む"],p "このファイルは従来のCatDB形式です。読み取り用に開いています。上の保存先に別のファイル名を入れ、元のデータを残したまま取り込めます。"
      ,field "取り込むデータの名前" (input "例：imported" (adoptNameInput m) (Edit AdoptNameField)),button "btn primary" "別ファイルに取り込む" (Guard AdoptLegacy) (isPending m)]
  ,if isJust (snapshotState m >>= statePath) then divc "stepc"
    [H.h2_ [] [txt "バックアップと書き出し"],p "バックアップには、すべての確定版、対応、ビューと由来が入ります。下書きは入りません。"
    ,divc "form-actions" [btn ".catdb をダウンロード" (Download "backup"),button "btn" "定義とデータをJSONで書き出す" (Download "export") (isJust (snapshotState m >>= stateLegacy))]
    ,field "別の場所にバックアップする" (input "新しい .catdb のパス" (backupInput m) (Edit BackupField)),button "btn" "新しいファイルへ保存" BackupDatabase (isPending m)
    ,hint "JSONはライブラリで再利用するための定義です。Studioのデータファイルは .catdb が正本です。"] else empty
  ,hint "PoCの対象は、有限で循環しないスキーマと、必ず行き先がある矢印です。一般的な数値列、NULL、SQL、集計は扱いません。見本の文章は架空の表示名です。"]

recordsPage :: Model -> V
recordsPage m = case snapshot m of
  Nothing -> divc "empty" [p "ファイルを開き、データを選んでください。",btn "ファイルを開く" (Navigate Files)]
  Just snap -> divc "record-page"
    [heading (if committedVisible m then "確定版 "<>snapshotRevision snap else if dirty m then "下書き · 元は "<>snapshotRevision snap else if snapshotDerived snap then "派生した版 "<>snapshotRevision snap else "確定版 "<>snapshotRevision snap)
      (focusedObject m) (if snapshotDerived snap then "計算した結果を、元の版への由来とともに見られます。" else "行を選ぶと、その行から続く道と行き先が見えます。")
    ,viewBanner m
    ,if dirty m then divc ("draft-status"<>if localProblems (currentSchema m) (workingData m)>0 then " invalid" else "")
      [txt (if verified m then "✓ 下書き全体の参照と約束を確認しました。まだ保存していません。「まとめて適用」で再確認し、一つの確定版として保存します。"
        else let n=localProblems (currentSchema m) (workingData m) in if n>0 then "≠ "<>ms n<>" 件の不一致や参照切れがあります。この下書きはまだ保存していません。参照先を修正してから「下書きを確認」してください。" else "変更はまだ保存していません。「下書きを確認」で、すべての参照と約束を確かめます。")
      ,field "変更の理由（任意）" (input "例：引用と本をまとめて変更" (reasonInput m) (Edit ReasonField))] else empty
    ,divc "table-tools" [input "レコードを検索" (search m) (Edit SearchField),H.span_ [] [txt (ms (length allRows)<>" 件")]]
    ,divc ("tablewrap"<>if snapshotDerived snap then " dvwrap" else "")
      [H.table_ [] [H.thead_ [] [H.tr_ [] (H.th_ [] [txt "行"]:[H.th_ [] [txt (arrowId a<>" → "<>arrowTo a)] | a <- fields])]
        ,H.tbody_ [] [H.tr_ [P.class_ (if focusedRow m==Just (recordId r) then "sel" else "")]
          (H.td_ [] [H.button_ [P.class_ "rowbtn",H.onClick (FocusRow (focusedObject m) (recordId r))]
            [H.span_ [P.class_ "rid"] [txt (recordId r)],recordCaption m (focusedObject m) (recordId r)]]
          :[cell r a | a <- fields]) | r <- take 100 filteredRows]]]
    ,if null filteredRows then hint "この条件に合う行はありません。" else if length filteredRows>100 then hint "先頭100件を表示しています。行IDや表示名を検索すると、ほかの行も選べます。" else empty
    ,if editingAllowed m then H.details_ [P.class_ "stepc add-record"]
      [H.summary_ [] [txt "＋ 行を追加する"],field "行ID" (input "重複しない名前" (newRecordInput m) (Edit NewRecordField))
      ,divc "field-grid" [field (arrowId a<>" → "<>arrowTo a) (select (arrowId a) (fromMaybe "" (lookup (arrowId a) (insertTargets m))) (SetInsertTarget (arrowId a))
        [(recordId r,recordId r<>" · "<>caption m (arrowTo a) (recordId r)) | r <- records (workingData m) (arrowTo a)]) | a <- fields]
      ,button "btn" "下書きに追加" AddRecord (isPending m),hint "すべての矢印の行き先が必要です。関連する行も、同じ下書きの中で先に追加できます。"]
      else if selectedView m==Nothing && activeSystem m==Nothing && snapshotRevision snap/="legacy" then divc "stepc"
        [p "この版は履歴のスナップショットです。編集するには、編集用の系として明示的に採用してください。"
        ,field "編集用の系の名前" (input "新しい名前、または切り替える既存の名前" (adoptNameInput m) (Edit AdoptNameField))
        ,button "btn" "この版を採用する" (Guard AdoptRevision) (isPending m),hint "同じ名前を指定すると、その系の確定版をこの版へ切り替えます。以前の版は履歴に残ります。"] else empty]
    where
      fields=outgoing (currentSchema m) (focusedObject m)
      allRows=records (visibleData m) (focusedObject m)
      filteredRows=filter (\r -> let q=T.toCaseFold (fromMisoString (search m)); body=T.toCaseFold (fromMisoString (recordId r<>" "<>caption m (focusedObject m) (recordId r))) in T.isInfixOf q body) allRows
      cell r a = let value=fromMaybe "未指定" (lookup (arrowId a) (recordFields r)); changed=fieldTarget (maybe [] snapshotData (snapshot m)) (focusedObject m) (recordId r) (arrowId a)/=Just value
        in H.td_ [P.class_ (if dirty m && changed && not (committedVisible m) then "chg" else "")]
          [H.button_ [P.class_ "cellbtn",H.onClick (if editingAllowed m then Pick (focusedObject m) (recordId r) (arrowId a) else FocusRow (arrowTo a) value)]
            [H.span_ [P.class_ "rid"] [txt value],recordCaption m (arrowTo a) value
            ,if dirty m && changed && not (committedVisible m) then H.span_ [P.class_ "tag"] [txt "変更"] else empty]]

viewBanner :: Model -> V
viewBanner m = case selectedView m >>= \key -> snapshotState m >>= find ((==key) . viewKey) . stateViews of
  Nothing -> empty
  Just view -> divc ("snap"<>if viewFreshness view=="stale" then " stale" else "")
    [txt (viewKey view<>" · 元の "<>viewInput view<>" から計算"<>if viewFreshness view=="stale" then " · 元の確定版が進みました。再計算するまで、この結果は計算に使った版のままです" else if viewFreshness view=="pinned" then " · この版に固定" else " · 最新")
    ,if isJust (fieldCourse m) && viewKey view `elem` ["1 顧客をそろえる","2 派遣の形で読む","3 担当できる組"]
      then button "btn small" "三段を再計算" (Guard (FieldStage "all")) (isPending m)
      else button "btn small" "再計算する" (Guard (RecomputeView (viewKey view))) (isPending m)]

structurePage :: Model -> V
structurePage m = divc "page-stack"
  [heading "レコードの形" "スキーマ" "オブジェクトは行の集まり、矢印は各行から必ず一つの行を選ぶ関係です。二つの道の約束は、その始点にあるすべての行に適用されます。"
  ,divc "schema-list" [schemaCard key def | (key,def) <- maybe [] stateSchemas (snapshotState m)]
  ,btn "新しいスキーマを作る" (Navigate SchemaEditor)
  ,mathNote m "スキーマは有限・非巡回な圏の表示です。宣言したパス等式で等しい道は、同じ射です。既存のスキーマは書き換えず、新しい名前のスキーマと対応を追加します。"]
schemaCard :: S -> WSchema -> V
schemaCard key def = divc "stepc"
  [H.h2_ [] [txt key],divc "object-chips" [H.code_ [] [txt o] | o <- schemaObjects def]
  ,H.ul_ [] [H.li_ [] [H.code_ [] [txt (arrowId a)],txt (" : "<>arrowFrom a<>" → "<>arrowTo a)] | a <- schemaArrows def]
  ,H.ul_ [] [H.li_ [] [H.code_ [] [txt (pathStart (equationLeft eq)<>" : "<>pathLabel (equationLeft eq)<>" = "<>pathLabel (equationRight eq))]] | eq <- schemaEquations def]]

schemaEditor :: Model -> V
schemaEditor m = divc "page-stack"
  [heading "空の形から作る" "スキーマを作る" "オブジェクトと矢印を置き、必要なら二つの道を等しいと宣言します。確認が通ると、空のレコードと編集用の系を一緒に保存します。"
  ,field "スキーマと編集用の系の名前" (input "例：my-project" (schemaNameInput m) (Edit SchemaNameField))
  ,divc "stepc" [H.h2_ [] [txt "1 オブジェクト"],divc "object-chips" [divc "chip" [txt o,button "small-link" "×" (RemoveObject o) False] | o <- schemaObjects def]
    ,divc "form-actions" [input "オブジェクト名" (newObjectInput m) (Edit NewObjectField),btn "追加" AddObject]]
  ,divc "stepc" [H.h2_ [] [txt "2 矢印"],H.ul_ [] [H.li_ [] [txt (arrowId a<>" : "<>arrowFrom a<>" → "<>arrowTo a),button "small-link" "削除" (RemoveArrow (arrowId a)) False] | a <- schemaArrows def]
    ,divc "field-grid" [field "矢印の名前" (input "例：belongsTo" (arrowNameInput m) (Edit ArrowNameField))
      ,field "始点" (select "始点" (arrowFromInput m) (Edit ArrowFromField) choices),field "終点" (select "終点" (arrowToInput m) (Edit ArrowToField) choices)]
    ,btn "矢印を追加" AddArrow,hint "同じオブジェクトへ戻る循環は作れません。矢印は、すべての行に行き先を要求します。"]
  ,divc "stepc" [H.h2_ [] [txt "3 二つの道の約束（任意）"],H.ul_ []
      [H.li_ [] [txt (pathLabel (equationLeft eq)<>" = "<>pathLabel (equationRight eq)),button "small-link" "削除" (RemoveEquation n) False] | (n,eq) <- zip [0..] (schemaEquations def)]
    ,field "共通の始点" (select "約束の始点" (equationStartInput m) (Edit EquationStartField) choices)
    ,pathBuilder "一つ目の道" def (WPath (equationStartInput m) (equationLeftInput m)) (AppendEquation True) (ClearEquation True)
    ,pathBuilder "二つ目の道" def (WPath (equationStartInput m) (equationRightInput m)) (AppendEquation False) (ClearEquation False)
    ,btn "この二つの道を等しいと宣言" AddEquation]
  ,button "btn primary" "確認してスキーマを保存" (Guard SaveSchema) (isPending m || blank (schemaNameInput m))
  ,hint "削除したオブジェクトや矢印を使う定義は自動修正しません。残った参照を直してから保存してください。"]
  where def=schemaDraft m; choices=[(o,o) | o <- schemaObjects def]

pathBuilder :: S -> WSchema -> WPath -> (S -> Action) -> Action -> V
pathBuilder label schema path append reset = divc "path-builder"
  [H.h3_ [] [txt label],divc "path" [H.span_ [P.class_ "pnode"] [txt (pathStart path)],H.code_ [] [txt (pathLabel path)],H.span_ [P.class_ "pnode"] [txt (fromMaybe "?" end)]]
  ,divc "nexts" [button "stepbtn" (arrowId a<>" → "<>arrowTo a) (append (arrowId a)) False | a <- maybe [] (outgoing schema) end]
  ,button "small-link" "道を最初から作る" reset False]
  where end=pathEndObject schema (pathStart path) (pathSteps path)

rulesPage :: Model -> V
rulesPage m = divc "page-stack"
  [heading "すべての始点の行に適用" "二つの道の約束" "一件だけ合えばよい約束ではありません。各行から二つの道をたどり、同じ行に着く必要があります。保存前には下書き全体を確認します。"
  ,if null (schemaEquations schema) then hint "このスキーマにはパス等式がありません。" else empty
  ,divc "table-tools" [input "行IDを検索" (search m) (Edit SearchField)]
  ,divc "page-stack" [divc "stepc"
      [H.h2_ [] [txt (pathStart (equationLeft eq)<>" : "<>pathLabel (equationLeft eq)<>" = "<>pathLabel (equationRight eq))]
      ,p (ms (length [() | (_,_,_,ok) <- checks,ok])<>" / "<>ms (length checks)<>" 行の道が合流")
      ,divc "lst" [H.button_ [H.onClick (FocusRow (pathStart (equationLeft eq)) key)]
        [H.span_ [P.class_ "rid"] [txt key],txt (caption m (pathStart (equationLeft eq)) key),H.span_ [P.class_ (if ok then "st ok" else "st bad")] [txt (if ok then "✓ 合流" else "≠ 不一致")]]
        | (_,_,key,ok) <- take 200 (filter (\(_,_,k,_) -> T.isInfixOf (fromMisoString (search m)) (fromMisoString k)) checks)]
      ,if length checks>200 then hint "一覧は検索結果の先頭200件です。上の合計は全行を調べています。" else empty]
    | (n,eq) <- zip [0..] (schemaEquations schema),let checks=filter (\(i,_,_,_) -> i==n) allChecks]
  ,mathNote m "パス等式は同じ射を指定します。たまたま今のデータで結果が同じという関係より強く、このスキーマ上のすべての適格なインスタンスに要求します。"]
  where schema=currentSchema m; allChecks=ruleChecks schema (visibleData m)

mappingsPage :: Model -> V
mappingsPage m = divc "page-stack"
  [heading "構造を別の構造へ対応させる" "保存した対応" "オブジェクトをオブジェクトへ、矢印を合成できる道へ送ります。スキーマの約束を保つことを確認してから保存します。"
  ,btn "新しい対応を作る" (Navigate MappingEditor)
  ,divc "page-stack" [divc "stepc"
    [H.h2_ [] [txt (mappingKey f<>" : "<>mappingFrom f<>" → "<>mappingTo f)]
    ,H.ul_ [] [H.li_ [] [txt (a<>" ↦ "<>b)] | (a,b) <- mappingObjects f]
    ,H.ul_ [] [H.li_ [] [txt (a<>" ↦ "<>pathLabel path)] | (a,path) <- mappingArrows f]
    ,btn "この対応をもとに新しく作る" (LoadMapping (mappingKey f))] | f <- maybe [] stateMappings (snapshotState m)]
  ,mathNote m "F : C → D は、パス等式を保つ関手です。D の等式で同じ射になる道は、表記が違っても同じ像です。同じオブジェクト像と同じ射の像を持つ対応は、同じ関手を表します。保存した入力表記は確認できます。"]

mappingEditor :: Model -> V
mappingEditor m = divc "page-stack"
  [heading "再利用できる対応" "スキーマの対応を作る" "まず二つのスキーマを選び、オブジェクトの対応先を一つずつ指定します。矢印は、対応先のスキーマ上で道を組み立てます。"
  ,field "対応の名前" (input "例：cards-by-quote" (mappingKey f) (Edit MappingNameField))
  ,divc "field-grid" [field "F の始点スキーマ" (select "F の始点" (mappingFrom f) (ChooseMappingSchema True) schemas)
    ,field "F の終点スキーマ" (select "F の終点" (mappingTo f) (ChooseMappingSchema False) schemas)]
  ,divc "stepc" [H.h2_ [] [txt "オブジェクトを送る"]
    ,divc "field-grid" [field (o<>" ↦") (select (o<>" の像") (fromMaybe "" (lookup o (mappingObjects f))) (ChooseObjectImage o) [(x,x) | x <- schemaObjects target]) | o <- schemaObjects source]]
  ,divc "stepc" [H.h2_ [] [txt "矢印を道へ送る"],divc "nexts"
      [button ("stepbtn"<>if selectedMappingArrow m==arrowId a then " chosen" else "") (arrowId a) (ChooseMappingArrow (arrowId a)) False | a <- schemaArrows source]
    ,if blank (selectedMappingArrow m) then hint "矢印を選んでください。矢印のないスキーマなら、道の指定は不要です。" else divc "mapping-path"
      [p ("必要な始点："<>expected True<>"　終点："<>expected False)
      ,pathBuilder "選んだ矢印の道" target (mappingSelectedPath m) AppendMappingPath ResetMappingPath
      ,button "btn small" "この始点の恒等の道にする" ResetMappingPath (blank (expected True))
      ,hint "空の道（恒等の道）も、上のボタンで明示的に指定してください。オブジェクトの像を変えても、道は自動修正しません。"]]
  ,button "btn primary" "対応を確認して保存" SaveMapping (isPending m || blank (mappingKey f))
  ,mathNote m "Δ は F:出力→入力、Σ と Π は F:入力→出力 を使います。保存時には射の始点・終点と、始点スキーマのすべてのパス等式を確認します。"]
  where
    f=mappingDraft m; source=fromMaybe (WSchema [] [] []) (mappingInputSchema m); target=fromMaybe (WSchema [] [] []) (mappingOutputSchema m)
    schemas=[(key,key) | (key,_) <- maybe [] stateSchemas (snapshotState m)]
    expected from=fromMaybe "" $ do a <- arrowDef source (selectedMappingArrow m); lookup (if from then arrowFrom a else arrowTo a) (mappingObjects f)

viewBuilder :: Model -> V
viewBuilder m = divc "page-stack"
  [heading "元の版と計算を残す" "ビューを作る" "保存した対応を使って、確定版から新しいデータを計算します。元のデータはそのまま残り、結果の各行から由来を開けます。"
  ,divc "stepc" [field "ビューの名前" (input "例：reading-cards" (viewNameInput m) (Edit ViewNameField))
    ,field "入力する確定版" (select "入力の版" (viewSourceInput m) (Edit ViewSourceField)
      [(revisionId r,revisionId r<>" · "<>revisionSchema r<>" · "<>ms (revisionCount r)<>"行"<>fromMaybe "" ((" · 現在の "<>) . fst <$> find ((==revisionId r) . snd) heads)) | r <- revisions])
    ,field "計算すること" (select "計算" (operationInput m) (Edit OperationField)
      [("delta","必要な構造で読み直す（Δ）"),("sigma","宣言した同一視でまとめる（Σ）"),("pi","関係がそろう組を集める（Π）")])
    ,p (case operationInput m of "delta" -> "カードから本への関係だけを取り出す、といった読み直しです。同じ本を指すカードも別々のまま残ります。"; "sigma" -> "指定した対応のもとで同一視される行をまとめます。同じ実在物であることを推定する機能ではありません。"; _ -> "すべての矢印の関係を満たす組を列挙します。組が作れることは、内容が事実であるという証明ではありません。")]
  ,divc "stepc" [H.h2_ [] [txt "使う対応と向き"]
    ,p (if operationInput m=="delta" then "F : 出力スキーマ → 入力スキーマ" else "F : 入力スキーマ → 出力スキーマ")
    ,field "確認済みの対応" (select "使う対応" (viewMappingInput m) ChooseViewMapping
      [(mappingKey f,mappingKey f<>" : "<>mappingFrom f<>" → "<>mappingTo f) | f <- eligibleMappings m])
    ,if null (eligibleMappings m) then divc "nexts" [p "この向きに使える対応がありません。",btn "対応を作る" (Navigate MappingEditor)] else empty
    ,hint (case find ((==viewSourceInput m) . snd) heads of Just (sys,_) -> "元の系「"<>sys<>"」に結び付けます。確定版が進むと古い版の表示になり、再計算するまで結果を保ちます。"; Nothing -> "選んだ入力の版に固定します。再計算しても、この版を使います。")]
  ,button "btn primary" "計算してビューを保存" (Guard RunView) (isPending m || blank (viewNameInput m) || blank (viewMappingInput m) || blank (viewSourceInput m))
  ,mathNote m "Δ_F は前合成、Σ_F は輸送トークンの商、Π_F はコンマ圏上の適合する族です。入力リビジョンと型付き証人をカタログに保存します。予算を超える計算は部分結果を保存しません。"]
  where heads=maybe [] stateHeads (snapshotState m); revisions=maybe [] stateRevisions (snapshotState m)

historyPage :: Model -> V
historyPage m = divc "page-stack"
  [heading "確定した版は残る" "版の履歴" "古い行やビューの由来は、そのとき使った版を開きます。現在の確定版へ移る操作とは区別します。"
  ,divc "tablewrap" [H.table_ [] [H.thead_ [] [H.tr_ [] [H.th_ [] [txt "版"],H.th_ [] [txt "スキーマ"],H.th_ [] [txt "行数"],H.th_ [] [txt "理由・由来"]]]
    ,H.tbody_ [] [H.tr_ [] [H.td_ [] [button "rowbtn" (revisionId r<>if revisionDerived r then " · 派生" else "") (Guard (OpenRevision (revisionId r) Nothing Nothing)) (isPending m)]
      ,H.td_ [P.class_ "stc"] [txt (revisionSchema r)],H.td_ [P.class_ "stc"] [txt (ms (revisionCount r))]
      ,H.td_ [P.class_ "stc"] [txt (revisionReason r),maybe empty (\prev -> hint ("前の版 "<>prev)) (revisionPrevious r)]] | r <- reverse (maybe [] stateRevisions (snapshotState m))]]]]

inspector :: Model -> V
inspector m = case (snapshot m,focusedRow m) of
  (Just snap,Just key) | isJust (record (visibleData m) (focusedObject m) key) -> divc "inspector-content"
    [divc "ih" [divc "kick" [txt (if dirty m && not (committedVisible m) then "下書きの道" else "選んだ行 · "<>snapshotRevision snap)]]
    ,H.h2_ [P.class_ "it"] [txt (caption m (focusedObject m) key)]
    ,case listToMaybe [eq | eq <- schemaEquations schema,pathStart (equationLeft eq)==focusedObject m] of
      Nothing -> node m (focusedObject m,key)
      Just eq -> equationDiagram m eq key
    ,pickerView m
    ,divc "sec" [H.h3_ [] [txt "矢印をたどる"],pathFollower m key]
    ,if editingAllowed m then divc "sec" [button "btn danger" "この行を下書きで削除" DeleteRecord (isPending m),hint "参照する行は自動削除しません。参照が残れば適用時に拒否されます。"] else empty
    ,if not (dirty m) then provenanceView m else hint "下書きは未保存です。確定版との比較は上の切り替えから見られます。"
    ,if not (dirty m) && snapshotRevision snap/="legacy" then divc "sec" [button "btn small" "この行と行き先から部分を作る" (Guard GeneratePart) (isPending m),hint "選んだ行から矢印をたどって閉じた部分を、元の版に固定したビューとして保存します。"] else empty]
  _ -> divc "empty" [H.h2_ [] [txt "一つの行から、道を見る。"],p "表の行を選んでください。関係を変更すると、下書きで道が合流するか確かめられます。",hint "元の行と派生した行、下書きと確定版は区別して表示します。"]
  where schema=currentSchema m

node :: Model -> Node -> V
node m (object,key) = H.button_ [P.class_ ("node "<>object),H.onClick (FocusRow object key)]
  [divc "nh" [txt object,H.span_ [P.class_ "rid"] [txt key]],if caption m object key==key then empty else divc "nt" [txt (caption m object key)]]

recordCaption :: Model -> S -> S -> V
recordCaption m object key = if caption m object key==key then empty else txt (caption m object key)

equationDiagram :: Model -> WEquation -> S -> V
equationDiagram m eq key = divc "equation-focus"
  [divc "sq" [node m (focusedObject m,key),route (equationLeft eq) a,route (equationRight eq) b,endpoints]
  ,H.p_ [P.class_ ("verdict "<>if ok then "ok" else "bad")]
    [H.b_ [] [txt (if ok then "✓ 二つの道が合流しています" else "≠ 二つの道が閉じていません")]
    ,txt (pathLabel (equationLeft eq)<>" と "<>pathLabel (equationRight eq)<>" は、すべての "<>focusedObject m<>" の行で同じ行き先を要求します。")]
  ,mathNote m "この二つの道は、宣言したパス等式により同じ射を表します。下書きが分岐した状態は、このスキーマのインスタンスとしては保存できません。"]
  where
    (a,b,ok)=equationCheck (currentSchema m) (visibleData m) eq key
    end result=either (const Nothing) lastNode result
    route path result=divc "col" (case result of
      Left why -> [divc "route-error" [txt why]]
      Right ns -> concat [[divc "arrow" [divc "ln" [],H.code_ [P.class_ "lbl"] [txt arrow],divc "ln" [],divc "hd" []]
                        ,if n<length (pathSteps path)-1 then node m at else empty] | (n,(arrow,at)) <- zip [0..] (zip (pathSteps path) (drop 1 ns))])
    endpoints=if ok then divc "ends one" [maybe empty (node m) (end a),H.span_ [P.class_ "cap"] [txt "同じ行に着く"]]
      else divc "ends two" [maybe (divc "node missing" [txt "行き先なし"]) (node m) (end a),H.span_ [P.class_ "neq"] [txt "≠"],maybe (divc "node missing" [txt "行き先なし"]) (node m) (end b)]

pickerView :: Model -> V
pickerView m = case picker m of
  Nothing -> empty
  Just (object,key,arrow) -> case arrowDef (currentSchema m) arrow of
    Nothing -> empty
    Just def -> divc "picker" [divc "pk-h" [txt (arrow<>" の行き先を選ぶ"),button "btn small" "閉じる" ClosePicker False]
      ,divc "picker-options" [H.button_ [P.class_ "opt",H.onClick (ChooseTarget (recordId r))]
        [H.span_ [P.class_ "rid"] [txt (recordId r)],recordCaption m (arrowTo def) (recordId r)
        ,H.span_ [P.class_ ("cons "<>if problems==0 then "ok" else "bad")]
          [txt (if problems==0 then "✓ 参照と二つの道の約束を保ちます" else "≠ "<>ms problems<>" 件の不一致・参照切れ")]]
        | r <- records (workingData m) (arrowTo def),let problems=localProblems (currentSchema m) (setTarget object key arrow (recordId r) (workingData m))]
      ,if null (records (workingData m) (arrowTo def)) then hint "行き先の行がありません。先に対象オブジェクトへ行を追加してください。" else empty]

pathFollower :: Model -> S -> V
pathFollower m key = divc "path-follow"
  [H.code_ [] [txt (pathLabel path)],case followed of
    Left why -> hint why
    Right ns -> divc "trace" (concat [[if n>0 then divc "ta" [txt "↓"] else empty,node m at] | (n,at) <- zip [0::Int ..] ns])
  ,divc "nexts" [button "stepbtn" (arrowId a<>" → "<>arrowTo a) (AppendPath (arrowId a)) False | a <- maybe [] (outgoing (currentSchema m)) end]
  ,if null (focusPath m) then empty else button "small-link" "選んだ行からたどり直す" ClearPath False]
  where path=WPath (focusedObject m) (focusPath m); followed=follow (currentSchema m) (visibleData m) path key
        end=pathEndObject (currentSchema m) (focusedObject m) (focusPath m)

provenanceView :: Model -> V
provenanceView m = case explanation m of
  Nothing -> empty
  Just e -> divc "sec provenance"
    [H.h3_ [] [txt "この行の由来"],p description
    ,mathNote m (explanationKind e)
    ,divc "lst" [H.button_ [H.onClick (Guard (OpenRevision (refRevision r) (Just r) Nothing))]
      [divc "origin-copy" [H.span_ [P.class_ "origin-revision"] [txt (refRevision r<>" 時点の "<>refObject r)],H.code_ [] [txt (refRecord r)]
        ,maybe empty (\path -> H.small_ [] [txt (pathLabel path)]) (originPath origin)
        ,maybe empty (\slot -> H.small_ [] [txt ("族の成分 "<>slot)]) (originSlot origin)]]
      | origin <- explanationOrigins e,let r=originRef origin]
    ,if explanationKind e=="PiFamily" && null (explanationOrigins e) then divc "empty-family" [txt "∅ 空の族 — 必要な成分がない位置にできる一つの空の組です。観測したレコードではありません。"] else empty
    ,if null (explanationAnchors e) then empty else H.details_ [] [H.summary_ [] [txt "保存されたアンカーID"],H.ul_ [] [H.li_ [] [H.code_ [] [txt a]] | a <- explanationAnchors e]]
    ,case selectedView m >>= \key -> snapshotState m >>= find ((==key) . viewKey) . stateViews >>= viewSystem of
      Nothing -> empty
      Just sys -> case snapshotState m >>= lookup sys . stateHeads of
        Nothing -> empty
        Just rev -> button "small-link" ("元の系の現在の確定版 "<>rev<>" を開く") (Guard (OpenRevision rev Nothing Nothing)) (isPending m)]
    where description=case explanationKind e of
            "PullRef" -> "元の一行を別の構造で読んでいます。参照先は計算に使った版です。"
            "SigmaClass" -> "指定した対応のもとで同一視された、輸送元の行と道です。実在物の同一性を保証するものではありません。"
            "PiFamily" -> "すべての矢印の関係を満たす組です。各成分を、使った版までたどれます。組が作れることは内容の真実性を保証しません。"
            "GeneratedRef" -> "選んだ行から矢印に沿って閉じた部分に含まれる行です。"
            _ -> "この版に直接保存された行です。"

confirmView :: Model -> V
confirmView m = case confirm m of
  Nothing -> empty
  Just _ -> divc "modal-backdrop" [H.section_ [P.class_ "confirm-dialog",P.role_ "dialog",P.aria_ "modal" "true",P.aria_ "label" "下書きの扱い"]
    [H.h2_ [] [txt "下書きを破棄して進みますか"],p "この画面の変更はまだ保存していません。確定版はそのままです。"
    ,divc "form-actions" [btn "下書きに戻る" KeepDraft,button "btn primary" "破棄して進む" ConfirmDiscard False]]]
