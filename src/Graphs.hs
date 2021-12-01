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

findGraph :: Unit -> Graph Unit -> Maybe (Graph Unit)
findGraph u Empty = Nothing
findGraph u (Node e1 n e2)
    | u == n = Just (Node e1 n e2)
    | otherwise = result
    where result = getFirst $ mconcat results
          results = map (First . findGraph u) $ graphs
          graphs = map (\e -> dest e) (e1 ++ e2)

idEdge :: Graph Unit -> Edge Unit
idEdge idGr = edge idGr unitConv

edge :: Graph Unit -> Conversion -> Edge Unit
edge gr s = Edge {dest=gr, conv=s}

-- Identity graph with self reference
idGraph :: Unit -> Graph Unit
idGraph u = newGraph (u, u, unitConv)

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

move :: Graph Unit -> Graph Unit
move (Node _ u (n:ns)) = dest n

getNode :: Graph Unit -> Unit
getNode (Node _ n _) = n
