module Course (coursePage,coursePanel,courseRail,courseOrigins) where

import Data.List (find)
import Data.Maybe (fromMaybe,listToMaybe)
import Miso (View,text)
import qualified Miso.Html as H
import qualified Miso.Html.Property as P
import Miso.String (ms)
import Model
import Studio.Wire

type V=View Model Action
box :: S -> [V] -> V
box cls=H.div_ [P.class_ cls]
t :: S -> V
t=text
p :: S -> V
p value=H.p_ [] [t value]
empty :: V
empty=H.span_ [] []
button :: S -> S -> Action -> Bool -> V
button cls label action blocked=H.button_ ([P.class_ cls,H.onClick action]++[P.disabled_ | blocked]) [t label]
btn :: Model -> S -> Action -> V
btn m label action=button "btn" label (Guard action) (isPending m)
names :: [S]
names=["1 顧客をそろえる","2 派遣の形で読む","3 担当できる組"]

courseRail :: Model -> V
courseRail m=H.nav_ [P.class_ "rail course-rail",P.aria_ "label" "現場サービスの体験"]
  ([box "rh" [t "現場サービスの体験"],button "ri on" "体験の道順" (Navigate Course) False
   ,button "ri" "原本を読む" (Guard (GuideOpenSource "依頼" Nothing)) (isPending m)]
  ++ [button "ri view" (name<>maybe " · 未計算" (\v->if viewFreshness v=="stale" then " · 古い版" else "") view)
        (maybe (Navigate Course) (\v->Guard (OpenRevision (viewOutput v) Nothing (Just name))) view) (isPending m)
     | name<-names,let view=fieldView name m]
  ++ [box "rh" [t (if page m==Records then fieldStageName m<>"の表" else "調べる")]]
  ++ [button ("ri"<>if focusedObject m==o then " on" else "") o (FocusObject o) False | page m==Records,o<-schemaObjects (currentSchema m)]
  ++ [button "ri" "スキーマを見る" (Navigate Structure) False,button "ri" "三つの対応を見る" (Navigate Mappings) False
     ,button "ri" "版の履歴" (Navigate History) False,button "ri" "ファイル・バックアップ" (Navigate Files) False
     ,box "railfoot" [button "ri" "汎用ツールも表示" ToggleGuideTools False
       ,button "switch" (if math m then "数学の名前：表示中" else "数学の名前も見る") ToggleMath False
       ,p "架空の業務データです。原本・計算結果・未保存の変更を区別して扱います。"]])

coursePage :: Model -> V
coursePage m=case fieldCourse m of
  Nothing->box "empty" [p "現場サービスのデータベースを開いてください。"]
  Just course->box "page-stack course-page"
    [box "ph" [box "kick" [t "現場サービス · 実際のDBで試す"],H.h1_ [] [t "この保守依頼に、誰を派遣できるか？"]
      ,p "営業、請求、保守で顧客IDが違う。契約、資格、稼働予定も別の表です。原本を残して顧客をそろえ、必要な道を読み、条件がそろう担当候補を計算します。"]
    ,box "course-facts" [fact "依頼" (count "依頼") ,fact "担当者" (count "担当者"),fact "資格" (count "資格"),fact "稼働枠" (count "稼働枠")]
    ,p ("現在の原本："<>courseSource course<>"。ここに出る件数と候補は、開いている .catdb の確定したデータです。まだ計算していない結果は表示しません。")
    ,step "0" "別々の帳票を確かめる"
      [p ("営業顧客 "<>count "営業顧客"<>"件・請求顧客 "<>count "請求顧客"<>"件・保守顧客 "<>count "サポート顧客"<>"件。各IDは、明示した顧客キーを指しています。")
      ,p "「青葉メディカル」と「アオバ本社」は同じK01を指します。似た名前の「青葉メディカルサービス」は別のK02です。名前から同一性を推測する実験ではありません。"
      ,actions [btn m "保守側のIDと対応キーを見る" (GuideOpenSource "サポート顧客" Nothing),btn m "依頼と契約を読む" (GuideOpenSource "依頼" (Just "依頼01｜青葉の定期点検"))]]
    ,stageCard m "1" "顧客をそろえる" "Σ" "sigma" "1 顧客をそろえる" True
      "確認した対応表を使い、営業・請求・保守の別名を同じ顧客へまとめます。契約や依頼の行は別々に残ります。まとまった顧客を選ぶと、元のIDと道を確認できます。"
    ,stageCard m "2" "派遣に使う形で読む" "Δ" "delta" "2 派遣の形で読む" (current "1 顧客をそろえる")
      "派遣に必要な7種類の表を読みます。「依頼 → 顧客 → 地域」という二段の道を、依頼の地域 tr として読み直します。ここでは候補を結合したり、行を選別したりしません。"
    ,stageCard m "3" "条件がそろう組を計算する" "Π" "pi" "3 担当できる組" (current "2 派遣の形で読む")
      "依頼・資格・稼働枠の三つを組にします。製品、訪問枠、地域、担当者の四つが一致する組だけが候補になります。計算結果のct・cq・caから各成分へ戻れます。"
    ,case courseRuns course of
      []->box "course-result empty-result" [H.h2_ [] [t "まだ候補は計算していません"],p "上のΣ、Δ、Πを順に実行してください。入力した原本から、その都度実際に計算します。"]
      latest:previous->resultSection m course latest (listToMaybe previous)
    ,step "4" "資格を一つ追加して、結果を変える"
      [p "依頼11は東・月曜午後のセンサー交換です。佐藤の稼働枠はありますが、初期の原本には佐藤のセンサー資格がありません。資格を一行追加し、確認して保存してみましょう。"
      ,btn m "佐藤のセンサー資格を下書きに追加" (GuideMakeDraft "qualification")
      ,p "保存後、古い候補はそのまま残ります。三段を再計算して、どの依頼の候補が増えたか比べます。"
      ,button "btn primary" "現在の原本から三段を再計算" (Guard (FieldStage "all")) (isPending m)]
    ,step "5" "矛盾する変更が、保存前に止まるところを見る"
      [p "依頼01の製品だけを変える下書きを作ります。契約の製品と食い違うため、確認は拒否されます。次に、その製品に合う契約へ明示的に付け替え、二つの変更をまとめて適用します。"
      ,btn m "依頼01の製品だけを変える下書きを作る" (GuideMakeDraft "broken")]
    ,step "6" "残った由来と、持ち出す範囲を確かめる"
      [p "候補を一つ開き、右の由来から元の依頼・資格・稼働枠へ戻れます。古い結果は計算に使った版を開きます。「この行と行き先から部分を作る」では、その候補から矢印で到達する閉じた部分を保存します。"
      ,actions [button "btn" "履歴で以前の版を見る" (Navigate History) False,button "btn" "ファイルを閉じる・開く・書き出す" (Navigate Files) False]
      ,p ".catdbのバックアップには原本、各段の結果、由来と履歴が入ります。JSON書き出しはHaskellで再利用するための定義とデータです。"]
    ]
    where
      count object=ms (fromMaybe 0 (lookup object (courseCounts course)))
      current key=maybe False ((=="current") . viewFreshness) (fieldView key m)
      fact label value=box "course-fact" [H.strong_ [] [t value],H.span_ [] [t label]]

step :: S -> S -> [V] -> V
step number title content=H.section_ [P.class_ "stepc course-step"] (H.h2_ [] [H.span_ [P.class_ "no"] [t number],t title]:content)
actions :: [V] -> V
actions=box "form-actions"
stageCard :: Model -> S -> S -> S -> S -> S -> Bool -> S -> V
stageCard m number title symbol stage name ready description=step number title
  [p description,box "course-stage-state" [t (maybe "まだ計算していません" (\v->"保存済みの結果 "<>viewOutput v<>" · 入力 "<>viewInput v<>maybe "" (" · 原本 "<>) (viewCapturedSource v)<>(if viewFreshness v=="stale" then " · 原本が進んでいます" else "")) chosen)]
  ,actions [button "btn primary" (symbol<>" を実行する") (Guard (FieldStage stage)) (isPending m || not ready)
    ,maybe empty (\v->btn m "保存した結果を見る" (OpenRevision (viewOutput v) Nothing (Just name))) chosen]
  ,if ready then empty else p "先の段階を現在の原本から計算するか、下の「三段を再計算」を使ってください。"]
  where chosen=fieldView name m

resultSection :: Model -> WCourse -> WCourseRun -> Maybe WCourseRun -> V
resultSection m course run previous=H.section_ [P.class_ "course-result"]
  [H.h2_ [] [t (ms (length (courseRunRows run))<>" 組の派遣候補")]
  ,p ("この結果 "<>courseRunRevision run<>" は、原本 "<>courseRunSource run<>" から計算しました。"<>
    if courseRunSource run/=courseSource course then "現在の原本は "<>courseSource course<>"。再計算するまで、ここは古い版の結果です。" else "現在の原本に対応しています。")
  ,p "同じ担当者が複数の候補に現れます。これは各依頼に単独で適合する選択肢で、同時に割り当てて衝突しない計画や最適な割当を決めた結果ではありません。"
  ,box "tablewrap" [H.table_ [] [H.thead_ [] [H.tr_ [] [H.th_ [] [t "依頼"],H.th_ [] [t "担当者と条件"],H.th_ [] [t "実際の射影"]]]
      ,H.tbody_ [] [H.tr_ [] [H.td_ [] [button "rowbtn" (dispatchTicket row) (Guard (OpenRevision (courseRunRevision run) (Just (dispatchRow row)) (Just "3 担当できる組"))) (isPending m)]
        ,H.td_ [P.class_ "stc"] [H.strong_ [] [t (dispatchEngineer row)],H.small_ [] [t (dispatchProduct row<>" · "<>dispatchRegion row<>" · "<>dispatchSlot row)]
          ,H.small_ [] [t (dispatchQualification row)],H.small_ [] [t (dispatchAvailability row)]]
        ,H.td_ [P.class_ "stc"] [H.code_ [] [t (label<>" → "<>refRecord ref<>"　")] | (label,ref)<-dispatchProjections row]] | row<-courseRunRows run]]]
  ,box "course-ticket-counts" [H.span_ [P.class_ (if n==0 then "no-candidate" else "")]
      [t (ticket<>"："<>ms n<>"組")] | (ticket,n)<-courseTicketCounts course]
  ,maybe empty (comparison run) previous
  ,H.details_ [P.class_ "stepc near-miss"] [H.summary_ [] [t "惜しい組：依頼02 × 佐藤のポンプ資格 × 東・月曜午前の稼働"]
    ,p "現在の原本で、四つの条件を一つずつ比べます。初期状態では時刻・地域・担当者はそろい、製品だけが違います。"
    ,H.ul_ [] [H.li_ [] [t ((if conditionMatches c then "✓ " else "≠ ")<>conditionName c<>"："<>conditionLeft c<>" / "<>conditionRight c)] | c<-courseNearMiss course]
    ,p "条件を満たさない組は、Πの候補には入りません。原本を変更した場合は再計算した結果で確認します。"]]
  where
    identity row=(dispatchTicket row,dispatchQualification row,dispatchAvailability row)
    comparison current old=box "course-comparison"
      [H.h3_ [] [t ("前の結果 "<>courseRunRevision old<>" と比較")]
      ,p (ms (length (courseRunRows old))<>"組 → "<>ms (length (courseRunRows current))<>"組。生成されたp番号ではなく、元の依頼・資格・稼働枠の行IDで比べます。")
      ,H.ul_ [] [H.li_ [] [t ("＋ "<>dispatchTicket r<>" / "<>dispatchEngineer r<>" / "<>dispatchQualification r)]
        | r<-courseRunRows current,identity r `notElem` map identity (courseRunRows old)]
      ,H.ul_ [] [H.li_ [] [t ("− "<>dispatchTicket r<>" / "<>dispatchEngineer r)]
        | r<-courseRunRows old,identity r `notElem` map identity (courseRunRows current)]
      ,btn m "前の結果を、その版のまま開く" (OpenRevision (courseRunRevision old) Nothing Nothing)]

coursePanel :: Model -> V
coursePanel m=case fieldCourse m of
  Nothing->empty
  Just course->box "course-context"
    [box "course-context-head" [H.strong_ [] [t ("現場サービス · "<>fieldStageName m)],button "small-link" "体験の道順へ" (Navigate Course) False]
    ,case guideExercise m of
      Just "qualification" | dirty m -> p "佐藤のセンサー資格を、まだ保存していない下書きに追加しました。上の「下書きを確認」→「まとめて適用」の順に進みます。"
      Just "broken" | dirty m -> box "guide-repair"
        [p "製品と契約の道が食い違う下書きです。「下書きを確認」で拒否されることを確かめてください。次のボタンで、契約の付け替えも同じ下書きに追加します。"
        ,btn m "契約も、その製品に合う契約へ変更する" GuideRepair]
      _ -> case snapshotSchemaKey <$> snapshot m of
        Just "fs:normalized"->p "同じ対応キーの顧客がまとまり、契約や依頼は残っています。顧客の由来を見たら、道順に戻ってΔを実行します。"
        Just "fs:operations"->p "依頼の tr は、元の顧客を経由して読んだ地域です。次にΠで依頼・資格・稼働枠の条件をそろえます。"
        Just "fs:candidates"->p "候補のct・cq・caは、実際の依頼・資格・稼働枠を指します。右の由来から、計算に使った版の原本へ戻れます。"
        _->p ("現在の原本は "<>courseSource course<>"、いま開いている版は "<>maybe "—" snapshotRevision (snapshot m)<>" です。計算済みの結果は、原本を適用しても再計算するまで変わりません。")
    ,if any ((=="stale") . viewFreshness) (maybe [] stateViews (snapshotState m))
      then button "btn small" "現在の原本から三段を再計算" (Guard (FieldStage "all")) (isPending m) else empty
    ,if dirty m then empty else case snapshotSchemaKey <$> snapshot m of
      Just "fs:raw"->btn m "次：Σで顧客をそろえる" (FieldStage "sigma")
      Just "fs:normalized"->btn m "次：Δで派遣の形で読む" (FieldStage "delta")
      Just "fs:operations"->btn m "次：Πで担当できる組を計算" (FieldStage "pi")
      Just "fs:candidates"->button "btn" "全候補と体験の続きへ" (Navigate Course) False
      _->empty]

courseOrigins :: Model -> V
courseOrigins m=case selected of
  Nothing->empty
  Just row->box "sec course-origins" [H.h3_ [] [t "元の帳票へ直接戻る"]
    ,p "この候補の証人をΣ・Δの由来までたどった、実際の原本の行です。現在版に読み替えません。"
    ,box "lst" [H.button_ [H.onClick (Guard (OpenRevision (refRevision r) (Just r) Nothing))]
      [box "origin-copy" [H.small_ [] [t (refRevision r<>" · "<>refObject r)],H.span_ [] [t (refRecord r)]]]
      | r<-dispatchOrigins row,refObject r `elem` ["依頼","資格","稼働枠"]]]
  where selected=do
          course<-fieldCourse m; snap<-snapshot m; key<-focusedRow m
          find ((==WRef (snapshotRevision snap) (focusedObject m) key) . dispatchRow) (concatMap courseRunRows (courseRuns course))
