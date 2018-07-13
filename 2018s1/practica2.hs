--                                                      TP 2 - NICOLÁS MATÍAS GARCÍA

import Data.Maybe

--1.High Order Functions

apply :: (a -> b) -> a -> b
apply f x = f x  

twice :: (a -> a) -> a -> a
twice f = f . f 

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

(.?) :: (b -> c) -> (a -> b) -> (a -> c)
(.?) f g x = f (g x)

curry' :: ((a,b) -> c) -> a -> b -> c
curry' f x y = f (x,y)

uncurry' :: (a -> b -> c) -> (a,b) -> c
uncurry' f (x,y) = f x y

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs) = if f x then x: filter' f xs else filter' f xs


any',all' :: (a -> Bool) -> [a] -> Bool

any' f [] = False
any' f (x:xs) = f x || any' f xs

all' f [] = True
all' f (x:xs) = f x && all' f xs

maybe' :: b -> (a -> b) -> Maybe a -> b
maybe' x f (Just y) = f y
maybe' x f Nothing = x


either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g (Left y) = f y
either' f g (Right x) = g x


find' :: (a -> Bool) -> [a] -> Maybe a
find' f [] = Nothing
find' f (x:xs) = if f x then Just x else find' f xs

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition f [] = ([],[])
partition f (x:xs) = let (left,right) = partition f xs
                               in if f x then ((x:left),right) else (left,(x:right))


nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy f [] = []
nubBy f (x:xs) = x : nubBy f (filter' (\y -> not (f x y)) xs)   


groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' f [] =  []
groupBy' f (x:xs) = (x:ys) : groupBy' f zs
    where (ys,zs) = span (f x) xs


deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy f x [] = []
deleteBy f x0 (x1:xs) = if f x0 x1 then xs else x1 : deleteBy f x0 xs 


concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f [] = []
concatMap' f (x:xs) = f x ++ concatMap' f xs


until' :: (a -> Bool) -> (a -> a) -> a -> a 
until' f g x = if f gApply then gApply else until' f g gApply where gApply = g x


takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) = if f x then x: takeWhile' f xs else []


dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f l@(x:xs) = if f x then dropWhile' f xs else l


span', break' :: (a -> Bool) -> [a] -> ([a],[a])

span' f xs = (takeWhile' f xs, dropWhile' f xs) 

break' f xs = span' (not . f) xs

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] [] = []
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys 


zipApply = zipWith ($)
  

index :: [a] -> [(Int,a)]
index [] = []
index (x:xs) = (0,x) : map' (\(x,y) -> (x+1,y)) (index xs)

index' xs = zip [0..] xs
  

applyN :: Int -> (a -> a) -> a -> a
applyN 0 f x = x
applyN n f x = applyN (n-1) f (f x)


iterate' :: (a -> a) -> a -> [a]
iterate' f x = f x : iterate' f (f x)


findIndex :: (a -> Bool) -> [a] -> Maybe Int    
findIndex _ [] = Nothing
findIndex f (x:xs) = let maybe = findIndex f xs
                     in if f x then Just 0
                        else if isJust maybe then Just(fromJust(maybe) + 1)
                             else maybe


---------------------------------------------------------------------------------------------------------------------------------------------

--2. Currificación
--1)


id' :: a -> a
id' x = x 

const' :: a -> b -> a
const' x = (\y -> x)

fst' :: (a,b) -> a
fst' (x,y) = x 

snd' :: (a,b) -> b
snd' (x,y) = y 

swap' :: (a,b) -> (b,a)
swap' (x,y) = (y,x)

head' :: [a] -> a
head' [] = error "No hay head de una lista vacia"
head' (x:_) = x

tail' :: [a] -> [a]
tail' [] = error "No se puede hacer tail de una lista vacia"
tail' (_:xs) = xs
 

sum', product' :: Num a => [a] -> a

sum' = foldl (+) 0

product' = foldl (*) 1

elem', notElem' :: Eq a => a -> [a] -> Bool

elem' = any' . (==)

notElem' = all' . (/=)

and', or' :: [Bool] -> Bool

and' = all' id'

or' = any' id'

last' :: [a] -> a
last' = head . reverse 

init' :: [a] -> [a]
init' = reverse . tail . reverse

subset' :: Eq a => [a] -> [a] -> Bool
subset' ys xs = all (`elem` xs) ys
    

(++¿) :: [a] -> [a] -> [a]
(++¿) [] [] = []
(++¿) xs [] = xs
(++¿) [] ys = ys
(++¿) (x:xs) ys = x: (xs ++¿ ys)
 
(!!¿) :: [a] -> Int -> a
(!!¿) [] n = error "La lista no tiene dimensión n" 
(!!¿) (x:_) 0 = x 
(!!¿) (x:xs) n = xs !!¿ (n-1)


concat' :: [[a]] -> [a]
concat' xss = foldl (++) [] xss


take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x: take' (n-1) xs  

drop' :: Int -> [a] -> [a]
drop' n = applyN n tail


zip' :: [a] -> [b] -> [(a,b)]
zip' = zipWith' (,)

unzip' :: [(a,b)] -> ([a],[b])
unzip' xs = (map fst xs, map snd xs)


splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n  xs = (take n xs, drop n xs)   


maximum', minimum' :: Ord a => [a] -> a

maximum' = foldr1 max

minimum' = foldr1 min


lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' key = foldr f z
        where f (x,y) v = if key == x then Just y else v ; z = Nothing


tails :: [a] -> [[a]]
tails [] = []
tails [x] = []
tails (x:xs) = xs : tails xs

replicate' :: Int -> a -> [a]
replicate' n x = take n (repeat x)

repeat' :: a -> [a]
repeat' x = iterate id x

cycle' :: [a] -> [a]
cycle' xs = xs ++ cycle' xs

nats :: [Int]
nats = [1..]

agrupar :: Eq a => [a] -> [[a]]
agrupar = groupBy' (==)
 
length' :: [a] -> Int
length' = foldr onePlus 0
          where onePlus i j = 1 + j
 

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

factorial :: Int -> Int
factorial 1 = 1
fcatorial n = n * factorial (n-1)

----------------------------------------------------------------------------------------------------------------------------------------------
  
