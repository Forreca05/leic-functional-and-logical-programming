type Match = ((String,String), (Int,Int))
type MatchDay = [Match]
type League = [MatchDay]
myLeague :: League
myLeague = [[(("Porto","Sporting"),(2,2)),(("Benfica","Vitoria SC"),(4,0))], [(("Porto","Benfica"),(5,0)),(("Vitoria SC","Sporting"),(3,2))],[(("Vitoria SC","Porto"),(1,2)),(("Sporting","Benfica"),(2,1))]]

winner :: Match -> String
winner ((w1,w2),(s1,s2)) 
    | s1 > s2 = w1
    | s2 > s1 = w2
    | otherwise = "draw"

matchDayScore :: String -> MatchDay -> Int
matchDayScore team [] = 0
matchDayScore team (((h,a),(hs,as)):xs) 
    | team `elem` [h,a] = getScore team ((h,a),(hs,as))
    | otherwise = matchDayScore team xs

getScore :: String -> Match -> Int
getScore team x
    | winner x == "draw" = 1
    | winner x == team = 3
    | otherwise = 0

leagueScore :: String -> League -> Int
leagueScore t = foldr (\d acc -> matchDayScore t d + acc) 0

sortByCond :: Ord a => [a] -> (a -> a -> Bool) -> [a]
sortByCond [] _ = []
sortByCond [x] _ = [x]
sortByCond l cmp = merge (sortByCond l1 cmp) (sortByCond l2 cmp) cmp
   where (l1 ,l2) = splitAt (div (length l) 2) l

merge :: Ord a => [a] -> [a] -> (a -> a -> Bool) -> [a]
merge [] l _ = l
merge l [] _ = l
merge (x:xs) (y:ys) cmp
   | cmp x y = x:(merge xs (y:ys) cmp)
   | otherwise = y:(merge (x:xs) ys cmp)

numMatchDaysWithDraws :: League -> Int
numMatchDaysWithDraws = length . filter (any ((== "draw") . winner))

bigWins :: League -> [(Int,[String])]
bigWins league = [(i,[winner ((h,a),(hs,as)) | ((h,a),(hs,as)) <- matchDay, abs(hs - as) >= 3]) | (i,matchDay) <- zip [1..] league]

type City = String
type Path = [City]
type Distance = Int

type RoadMap = [(City,City,Distance)]

gTest1 :: RoadMap
gTest1 = [("0","1",10),("0","2",15),("0","3",20),("1","2",35),("1","3",25),("2","3",30)]

gTest2 :: RoadMap 
gTest2 = [("0","1",4),("2","3",2)]

adjacent :: RoadMap -> City -> [(City,Distance)]
adjacent [] _ = []
adjacent ((x,y,d):xs) s
    | x == s = (y,d) : adjacent xs s
    | otherwise = adjacent xs s

areConnected ::  RoadMap -> City -> City -> Bool
areConnected r x y 
    | y `elem` (getAdjacents (adjacent r x)) = True
    | otherwise = False

getAdjacents :: [(City,Distance)] -> [City]
getAdjacents [] = []
getAdjacents ((x,y):xs) = x : getAdjacents xs

