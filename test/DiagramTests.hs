module DiagramTests (diagramTests) where

import Control.Monad (unless,forM_)
import Data.List (nub)
import DiagramLayout

diagramTests :: IO ()
diagramTests=do
  let assert label condition=unless condition (fail ("diagram: "++label))
      arrows :: [(String,String,String)]
      arrows=[("book","Note","Book"),("quote","Note","Quote"),("quoteBook","Quote","Book")]
  assert "ordered composite retains intermediate object" (pathNodes arrows "Note" ["quote","quoteBook"]==Right ["Note","Quote","Book"])
  assert "empty path is identity" (pathNodes arrows "Quote" []==Right ["Quote"])
  assert "non-composable click rejected" (pathNodes arrows "Note" ["quoteBook"]==Left (NotComposable "quoteBook"))
  assert "unknown generator rejected" (pathNodes arrows "Note" ["missing"]==Left (UnknownArrow "missing"))
  let empty=layoutDag ([]::[Int]) [] []
  assert "empty graph has usable extent" (layoutWidth empty>0 && layoutHeight empty>0 && null (layoutNodes empty))
  let denseEdges=[(i,j)|i<-[0..17::Int],j<-[i+1..min 17 (i+3)]]
      dense=layoutDag [0..17::Int] denseEdges []
      positions=[(n,p)|Placed n p<-layoutNodes dense]
  assert "bounded dense DAG positions all nodes once" (length positions==18 && length (nub (map snd positions))==18)
  forM_ denseEdges $ \(a,b)->case (lookup a positions,lookup b positions) of
    (Just p,Just q)->assert "every DAG edge advances a layer" (pointX p<pointX q)
    _->fail "missing layout node"
  let pinned=layoutDag [0..17::Int] denseEdges [(3,Point 333 444)]
  assert "manual position survives additions and layout" (lookup 3 [(n,p)|Placed n p<-layoutNodes pinned]==Just (Point 333 444))
  let a=Point 70 230;b=Point 640 230
  assert "parallel edges have separate routes and labels" (routeEdge a b 0 2/=routeEdge a b 1 2 && routeLabel (routeEdge a b 0 2)/=routeLabel (routeEdge a b 1 2))
  assert "cycle drafts still draw without divergence" (length (layoutNodes (layoutDag [1,2::Int] [(1,2),(2,1)] []))==2)
  let cards=layoutVertical ["Card","Book"::String] [("Card","Book")] []
      reading=layoutVertical ["Note","Quote","Book"::String] [("Note","Quote"),("Quote","Book"),("Note","Book")] []
  assert "two reading schemas fit the compact migration width" (layoutWidth cards+85+layoutWidth reading<=850)
  let verticalPositions=[(n,p)|Placed n p<-layoutNodes reading]
  case (lookup "Note" verticalPositions,lookup "Quote" verticalPositions,lookup "Book" verticalPositions) of
    (Just n,Just q,Just b)->assert "compact migration layers advance downward" (pointY n<pointY q && pointY q<pointY b)
    _->fail "missing compact node"
  putStrLn "PASS: diagram paths, identity/invalid paths, empty/dense DAG, parallel/long routes and retained positions"
