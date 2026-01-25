type Movie = (String, [String])

myMovies :: [Movie]
myMovies = [("Titanic", ["Kate Winslet","Leonardo DiCaprio"]), ("Revolutionary Road",["Kate Winslet","Leonardo DiCaprio"]), ("Elektra ",["Jennifer Garner"]), ("The Great Gatsby",["Leonardo DiCaprio"])]

delAll :: Eq a => a -> [a] -> [a]
delAll _ [] = []
delAll x (y:ys)
    | x == y    = delAll x ys
    | otherwise = y : delAll x ys

uniqueVals :: Eq a => [a] -> [a]
uniqueVals [] = []
uniqueVals (x:xs) = x : uniqueVals (delAll x xs)

findActors :: [Movie] -> String -> Maybe[String]
findActors [] _ = Nothing
findActors ((x,y):xs) title
    | x == title = Just y
    | otherwise = findActors xs title

moviesWithActor :: [Movie] -> String -> [Movie]
moviesWithActor [] _ = []
moviesWithActor ((x,y):xs) actor 
    | elem actor y = (x,y) : moviesWithActor xs actor
    | otherwise = moviesWithActor xs actor


uniqueActors :: [Movie] -> [String]
uniqueActors movies = uniqueActorsAux movies []

uniqueActorsAux :: [Movie] -> [String] -> [String]
uniqueActorsAux [] acc = uniqueVals acc
uniqueActorsAux ((_, actors):xs) acc =
    uniqueActorsAux xs (acc ++ actors)

actorFrequencies :: [Movie] -> [(String,Int)]
actorFrequencies movies = [(actor,total) | actor <- uniqueActors movies,let total = length(moviesWithActor movies actor)]

predi :: [a] -> Int
predi [] = 0
predi (x:xs) = -1 + predi xs

evenPos :: Eq a => [a] -> [a] -> [(a,a)]
evenPos xs ys =
    [(s,d) | (i,(s,d)) <- zip [0..] (zip xs ys), i `mod` 2 == 0, s/=d]



