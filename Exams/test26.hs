type Node = String   -- some city
type Dist = Int      -- some distance
type Edges = [(Node,Node,Dist)] -- directed connections

portugal :: Edges
portugal = [("Porto", "Aveiro", 76) , ("Aveiro", "Coimbra", 63), ("Aveiro", "Leiria", 117), ("Coimbra", "Leiria", 76), ("Leiria", "Santarem", 83), ("Santarem", "Lisboa", 82)]

inverse :: Edges -> Edges 
inverse xs = [(b,a,d) | (a,b,d) <- xs]

newEdges :: Edges -> Edges 
newEdges xs = [(a,c,d) | (a,b1,d1) <- xs, (b2,c,d2) <- xs, let d = d1 + d2, b1 == b2, a /= c]

--pathDistance :: Edges -> [Node] -> Maybe Dist
--pathDistance e [] -> Just 0
--pathDistance [] _ -> Nothing
--pathDistance (

shortest :: Edges -> Edges
shortest [] = []
shortest e = help e e

help :: Edges -> Edges -> Edges
help [] e = []
help [(a,b,d)] e
   | ((minimumm (a,b,d) e) == d) = (a,b,d) : []
   | otherwise = []
help ((a,b,d):xs) e 
    | ((minimumm (a,b,d) e) == d) = (a,b,d) : help xs e
    | otherwise = help xs e

minimumm :: (Node,Node,Dist) -> Edges -> Dist
minimumm (a,b,d) [] = d
minimumm (a1,b1,d1) ((a2,b2,d2):xs) 
    | a1 == a2 && b1 == b2 = min(d1 (minimumm (a2,b2,d2) xs))
    | otherwise = minimumm (a1,b1,d1) xs
