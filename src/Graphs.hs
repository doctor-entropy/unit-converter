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
          results = map (First . findGraph u) $ nonIdGrs
          nonIdGrs = filter ((/=n). getNode) graphs
          graphs  = map (\e -> dest e) (e1 ++ e2)

edge :: Graph Unit -> Conversion -> Edge Unit
edge gr s = Edge {dest=gr, conv=s}

mkIdGraph :: Unit -> Graph Unit
mkIdGraph u = 
    Node [inc] u [outg]
    where idg  = mkIdGraph u
          outg = edge idg unitConv
          inc  = edge idg invUnitConv
          invUnitConv = inverse unitConv

connectGraphs 
    :: Graph Unit
    -> Graph Unit
    -> Conversion 
    -> Graph Unit
connectGraphs
    gr1@(Node is1 u1 os1)
    gr2@(Node is2 u2 os2)
    conv = gr1'
    where
    gr2' = connectGraphs gr2 gr1 iconv
    iconv = inverse conv
    o'  = edge gr2' conv
    i'  = edge gr2' (inverse conv)
    gr1' = Node (i':is1) u1 (o':os1)

newGraph :: Relation -> Graph Unit
newGraph (f, t, conv) = connectGraphs idf idt conv
                    where idt = mkIdGraph t
                          idf = mkIdGraph f

addNode :: Graph Unit -> Unit -> Conversion -> Graph Unit
addNode gr u conv = connectGraphs gr idg conv
                    where idg = mkIdGraph u

mkGraph :: Relation -> Graph Unit -> Graph Unit
mkGraph r@(f, t, conv) gr = 
    case findGraph f gr of
         Nothing  -> case findGraph t gr of
                          Nothing  -> newGraph r
                          Just gr' -> addNode gr' f iconv
         Just gr' -> addNode gr' t conv

    where iconv = inverse conv

moveF :: Graph Unit -> Graph Unit
moveF (Node _ u (o:os)) = dest o

moveR :: Graph Unit -> Graph Unit
moveR (Node (i:is) u _) = dest i

getNode :: Graph Unit -> Unit
getNode (Node _ n _) = n

outgEdges :: Graph Unit -> [Unit]
outgEdges (Node _ n os) = map (getNode . dest) os

incEdges :: Graph Unit -> [Unit]
incEdges (Node is n _) = map (getNode . dest) is

nonIdOutgEdges :: Graph Unit -> [Unit]
nonIdOutgEdges gr@(Node _ n _) = filter (/= n) $ outgEdges gr

nonIdIncEdges :: Graph Unit -> [Unit]
nonIdIncEdges gr@(Node _ n _) = filter (/= n) $ incEdges gr

pPrintGraph :: Graph Unit -> String
pPrintGraph gr@(Node is n os) = 
    "Node " ++ is' ++ " " ++ n ++ " " ++ os'
    where is' = show $ nonIdIncEdges gr
          os' = show $ nonIdOutgEdges gr
