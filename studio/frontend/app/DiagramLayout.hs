module DiagramLayout
  ( Point(..), Placed(..), Route(..), Layout(..), PathProblem(..), pathNodes, layoutDag, routeEdge, moveNode ) where

import qualified Data.List as List
import qualified Data.Map.Strict as M
import qualified Data.Set as Set

data Point = Point { pointX :: Double, pointY :: Double } deriving (Eq,Show)
data Placed n = Placed { placedKey :: n, placedPoint :: Point } deriving (Eq,Show)
data Route = Route { routeStart :: Point, routeControlA :: Point, routeControlB :: Point
                   , routeEnd :: Point, routeLabel :: Point } deriving (Eq,Show)
data Layout n = Layout { layoutNodes :: [Placed n], layoutWidth :: Double, layoutHeight :: Double }
  deriving (Eq,Show)
data PathProblem a = UnknownArrow a | NotComposable a deriving (Eq,Show)

pathNodes :: (Eq a,Eq n) => [(a,n,n)] -> n -> [a] -> Either (PathProblem a) [n]
pathNodes arrows start steps=go start steps
  where
    go current []=Right [current]
    go current (key:rest)=case List.find (\(a,_,_)->a==key) arrows of
      Nothing->Left (UnknownArrow key)
      Just (_,source,target) | source/=current->Left (NotComposable key)
                            | otherwise->(current:) <$> go target rest

-- Coordinates are presentation state. No layout choice alters a schema or path.
layoutDag :: Ord n => [n] -> [(n,n)] -> [(n,Point)] -> Layout n
layoutDag input edges pinned = Layout positioned width height
  where
    nodes=Set.toAscList (Set.fromList input)
    known=Set.fromList nodes
    admitted=[(a,b) | (a,b)<-edges,a `Set.member` known,b `Set.member` known]
    ranks=rankLayers nodes admitted
    groups=M.fromListWith (++) [(M.findWithDefault 0 n ranks,[n]) | n<-reverse nodes]
    automatic=M.fromList [(n,Point (70+fromIntegral rank*285) (95+fromIntegral i*165+stagger rank members))
      | (rank,members)<-M.toAscList groups,(i,n)<-zip [0::Int ..] members]
    stagger rank members=if length members==1 && M.size groups<=4 && even rank then 140 else 0
    positioned=[Placed n (M.findWithDefault (Point 70 95) n (M.union (M.fromList pinned) automatic)) | n<-nodes]
    width=maximum (850:[pointX p+270 | Placed _ p<-positioned])
    height=maximum (640:[pointY p+330 | Placed _ p<-positioned])

rankLayers :: Ord n => [n] -> [(n,n)] -> M.Map n Int
rankLayers nodes edges=go Set.empty M.empty
  where
    go visited ranks
      | Set.size visited==length nodes=ranks
      | null ready=List.foldl' (\m n->M.insert n 0 m) ranks remaining
      | otherwise=go (Set.union visited (Set.fromList ready))
          (List.foldl' (\m n->M.insert n (maximum (0:[M.findWithDefault 0 a ranks+1 | (a,b)<-edges,b==n])) m) ranks ready)
      where remaining=filter (`Set.notMember` visited) nodes
            ready=filter (\n->all (`Set.member` visited) [a | (a,b)<-edges,b==n]) remaining

routeEdge :: Point -> Point -> Int -> Int -> Route
routeEdge (Point ax ay) (Point bx by) lane count = Route start ca cb end label
  where
    shift=(fromIntegral lane-(fromIntegral count-1)/2)*26
    start=Point (ax+198) (ay+54+shift/2)
    end=Point (bx-4) (by+54+shift/2)
    long=bx-ax>400
    bend=if long then max ay by+205+fromIntegral lane*34 else (ay+by)/2+54+shift*2
    ca=Point (ax+245) (if long then bend else ay+54+shift*2)
    cb=Point (bx-50) (if long then bend else by+54+shift*2)
    label=Point ((pointX start+3*pointX ca+3*pointX cb+pointX end)/8)
                ((pointY start+3*pointY ca+3*pointY cb+pointY end)/8-12)

moveNode :: Eq n => n -> Point -> [(n,Point)] -> [(n,Point)]
moveNode key point positions=(key,point):filter ((/=key) . fst) positions
