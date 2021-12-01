module Graphs where

import Data.Monoid

type Unit     = String

data Scalar = Int | Exp Int Int
data Op = Mult | Div
type Conversion = (Op, Int)

type Relation = (Unit, Unit, Conversion)

data Edge a  = Edge { dest :: Graph a
                    , conv :: Conversion
                    }

data Graph a = Empty | Node [(Edge a)] a [(Edge a)]


inverse :: Conversion -> Conversion
inverse (Mult, s) = (Div, s)
inverse (Div, s)  = (Mult, s)

unitConv :: Conversion
unitConv = (Mult, 1)

-- Current logic does not terminate
-- TODO: Make it terminate
findGraph :: Unit -> Graph Unit -> Maybe (Graph Unit)
findGraph u Empty = Nothing
findGraph u (Node e1 n e2)
    | u == n = Just (Node e1 n e2)
    | otherwise = result
    where result = getFirst $ mconcat results
          results = map (First . findGraph u) $ graphs
          graphs = map (\e -> dest e) (e1 ++ e2)

idEdgeF :: Graph Unit -> Edge Unit
idEdgeF idGr = edge idGr unitConv

idEdgeR :: Graph Unit -> Edge Unit
idEdgeR idGr = edge idGr (inverse unitConv)

idEdge :: Graph Unit -> Edge Unit
idEdge idGr = edge idGr unitConv

edge :: Graph Unit -> Conversion -> Edge Unit
edge gr s = Edge {dest=gr, conv=s}

-- Identity graph with self reference
idGraph :: Unit -> Graph Unit
idGraph u = newGraph (u, u, unitConv)

mkIdGraph :: Unit -> Graph Unit
mkIdGraph u = 
    Node [fromEdg] u [toEdg]
    where idGr    = mkIdGraph u
          fromEdg = idEdgeR idGr
          toEdg   = idEdgeF idGr

connectGraphs :: Graph Unit -> Conversion -> Graph Unit -> Graph Unit
connectGraphs gr1 conv gr2 = 
    ( Node (fromEdg:f1) u1 (toEdg:t1) )
    where (Node f1 u1 t1) = gr1
          (Node f2 u2 t2) = gr2
          iconv = inverse conv
          fromGr = connectGraphs gr2 iconv gr1
          fromEdg = edge fromGr conv
          toEdg = edge gr2 conv

-- connectGraphs :: Graph Unit -> Conversion -> Graph Unit -> Graph Unit
-- connectGraphs gr1@(Node f1 u1 t1) conv gr2@(Node f2 u2 t2) = 
--         (Node (f1 ++ [e2] u1 (t1 ++ [e1]))

--     where e1 = edge gr2 conv
--           e2 = edge (connectGraphs gr2 (inverse conv) gr1) conv

newGraph :: Relation -> Graph Unit
newGraph (from, to, conv) =
    Node [idEg] from [toEg]
    where toEg  = edge toGr iconv
          toGr = newGraph (to, from, iconv)
          iconv = inverse conv

          idEg  = idEdge idGr
          idGr = idGraph from

addNode :: Graph Unit -> Unit -> Conversion -> Graph Unit
addNode prevGr u conv = Node [prevEdge] u [idEg]
    where idGr     = idGraph u
          idEg     = idEdge idGr
          prevEdge = edge prevGr (inverse conv)

mkGraph :: [Relation] -> Graph Unit-> Graph Unit
mkGraph []     _  = Empty
mkGraph (e:es) gr = case findGraph from gr of
                         Nothing  -> newGraph e
                         Just gr' -> let gr'' = addNode gr' to conv
                                     in mkGraph es gr''
                    where (from, to, conv) = e

moveF :: Graph Unit -> Graph Unit
moveF (Node _ u (n:ns)) = dest n

moveR :: Graph Unit -> Graph Unit
moveR (Node (n:ns) u _) = dest n

getNode :: Graph Unit -> Unit
getNode (Node _ n _) = n

getEdgesF :: Graph Unit -> [Unit]
getEdgesF (Node fs n ts) = map (getNode . dest) ts

getEdgesR :: Graph Unit -> [Unit]
getEdgesR (Node fs n ts) = map (getNode . dest) fs
