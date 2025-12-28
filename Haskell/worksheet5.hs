import Set (empty, fromList,
            insert, member, size, height)   
--import Data.List

{-data List a = Empty | Cons a (List a)

toList :: [a]-> List a
toList [] = Empty
toList (x:xs) = Cons x (toList xs)

fromList :: List a -> [a]
fromList Empty = []
fromList (Cons x xs) = x : fromList xs-}

data Suit = Clubs | Spades | Hearts | Diamonds
  deriving (Show, Eq, Enum, Bounded, Ord)

data Face = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
          | Jack | Queen | King | Ace
  deriving (Show, Eq, Enum, Bounded, Ord)

type Card = (Suit, Face)

allCards :: [Card]
allCards = [(x,y)|x<-[Clubs .. Diamonds], y <- [Two .. Ace]]

cmp1 :: Card-> Card-> Ordering
cmp1 (x,xs) (y,ys) | compare x y == EQ = compare xs ys
                   | otherwise = compare x y

set1 = foldr insert empty [1..1000]
set2 = fromList [1..1000]

type Name = Char
type Env = [(Name, Bool)]
data Prop = Const Bool
        | Var Name
        | Not Prop
        | And Prop Prop
        | Imply Prop Prop
        | Or Prop Prop

eval :: Env -> Prop -> Bool
eval env (Const b) = b
eval env (Var x)
    = case lookup x env of
        Just b -> b
        Nothing -> error "undefined variable"
eval env (Not p) = not (eval env p)
eval env (And p q)
    = eval env p && eval env q
eval env (Imply p q)
    = not (eval env p) || eval env q
eval env (Or p q)
    = eval env p || eval env q

vars :: Prop -> [Name]
vars (Var p) = [p]
vars (Const _) = []
vars (Not p) = vars p
vars (And p q) = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q
vars (Or p q) = vars p ++ vars q

booleans :: Int-> [[Bool]]
booleans 0 = [[]]
booleans n = [b:bs | b <- [False, True], bs <- booleans(n-1)]

environments :: [Name] -> [Env]
environments names = map (zip names) (booleans (length names))

table :: Prop-> [(Env,Bool)]
table p = [(env, eval env p) | env <- environments (vars p)]

satisfies :: Prop-> [Env]
satisfies p = [env | env <- environments (vars p), eval env p == True]