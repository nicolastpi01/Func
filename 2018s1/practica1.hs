--                                                      TP 1 - NICOLÁS MATÍAS GARCÍA

id' :: a -> a
id' x = x

const' :: a -> b -> a
const' x y = x

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
sum' [] = 0
sum' (x:xs) = x + sum' xs

product' [] = 1
product' (x:xs) = x * product' xs

elem', notElem' :: Eq a => a -> [a] -> Bool

elem' _ [] = False
elem' a (x:xs) = x == a || elem' a xs

notElem' a xs = not (elem' a xs)

and', or' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

or' [] = False
or' (x:xs) = x || or' xs

last' :: [a] -> a
last' [] = error "No hay último elemento en una lista vacía"
last' (x:[]) = x
last' (_:xs) = last' xs 


init' :: [a] -> [a]
init' [] = error "No se puede devolver la lista que queda de sacar el head si no hay head"
init' [x] = []
init' (x:xs) = x: init' xs

subset :: Eq a => [a] -> [a] -> Bool
subset [] _ = True
subset _ [] = False
subset (x:xs) ys = x `elem'` ys && subset xs ys 


(++¿) :: [a] -> [a] -> [a]
(++¿) [] ys = ys
(++¿) (x:xs) ys = x: (xs ++¿ ys)

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++¿ concat' xs

(!!¿) :: [a] -> Int -> a
(!!¿) [] n = error "La lista no tiene dimensión n" 
(!!¿) (x:_) 0 = x 
(!!¿) (x:xs) n = xs !!¿ (n-1)

take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x: take' (n-1) xs

drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' n [] = []
drop' n (x:xs) = drop' (n-1) xs

zip' :: [a] -> [b] -> [(a,b)]
zip' [] [] = []
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys



splitAt' :: Int -> [a] -> ([a], [a])
splitAt' 0 ys = ([],ys)
splitAt' n (x:xs) = let (xss,yss) = splitAt' n xs
                    in (x: fst (splitAt' (n-1) xs), snd (splitAt' (n-1) xs))



maximum', minimum' :: Ord a => [a] -> a

maximum' [] = error "No hay max. de lista vacia"
maximum' [x] = x
maximum' (x:xs) = max x (maximum xs)


minimum' [] = error "No hay min. de lista vacia"
minimum' [x] = x
minimum' (x:xs) = min x (minimum' xs)


lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' _ [] = Nothing
lookup' a (x:xs) = if a == (fst x) then Just (snd x) else lookup' a xs   



unzip' :: [(a,b)] -> ([a],[b])
unzip' [] = ([],[])
unzip' (x:xs) = let (zs,ys) = unzip' xs
                in (fst x: fst (unzip' xs),snd x: snd (unzip' xs))


tails :: [a] -> [[a]]
tails [] = []
tails (x:xs) = xs : tails xs


replicate' :: Int -> a -> [a]
replicate' 0 a = []
replicate' n a = a : replicate' (n-1) a

repeat' :: a -> [a]
repeat' a = a : repeat' a

cycle' :: [a] -> [a]
cycle' [] = []
cycle' xs = xs ++ cycle' xs

nats :: [Int]
nats = [1..]

agrupar :: Eq a => [a] -> [[a]]
agrupar [] = []
agrupar [x] = [[x]]
agrupar (x:xs) = let (ys:yss) = agrupar xs
                               in if elem' x ys then (x:ys):yss else [x] : agrupar xs

countBy :: (a -> Bool) -> [a] -> Int
countBy p  [] = 0
countBy p (x:xs) = 
  if p x then 1 + countBy p xs else countBy p xs



------------------------------------------------------------------------------------------------------------------------------------------------

--2

--1. Dar tipo a las siguientes expresiones y funciones

--data Either a b = Left a | Right b deriving Show

--a) True :: Bool
--b) [2] :: Num t => [t]
--c) Maybe ["Jorge"] :: NI IDEA
--d) Nothing :: Maybe a 
--e) [] :: [t]
--f) let x = [] in x ++ x :: [a]
--g) let f x = f x in f [] :: t
--h) data Either a b = Left a | Right b :: NI IDEA
--x = Left True :: Either Bool b
--y = Right (Left []) :: Either a (Either [t] b)
--z = Right (Left [Right []]) :: Either a (Either [Either a1 [t]] b)
--i) (:) :: a -> [a] -> [a]
--j) Maybe Ni idea
--k) Right :: b -> Either a b
--l) (1:) :: Num a => [a] -> [a]
--m) error "ups" :: Exception: ups
--n) error :: [Char] -> a
--ñ) undefined :: t
--o) undefined undefined :: t


--2. Dar ejemplos de expresiones que posean los siguientes tipos:
--a) Bool :: True || False
--b) (Int, Int) :: (1,2)
--c) Int -> Int -> Int :: (+)
--d ) a -> a :: (id)
--e) a :: NI IDEA
--f ) String -> a :: Ni idea
--g) a -> b :: NI IDEA


--3.
--Patterns
--Indicar si los siguientes patterns son correctos
--1. (x, y) Sí
--2. (1, y) Sí
--3. (n+1) Sí
--4. (’a’,(’a’,b)) Sí
--5. (a,(a,b)) Sí
--6. ([]:[4]) No
--7. (x:y:[]) Sí
--8. [x] Sí 
--9. ([]:[]) No





--5.
--Terminación
--Indicar qué programas terminan
--1. let nats = [1..] in nats No termina
--2. take 5 [1..] Sí termina
--3. let appendedNats = [1..] ++ [1..] in take 5 appendedNats Sí termina
--4. let x = x in x No termina
--5. undefined NO SÉ
--6. undefined undefined NO SÉ



--6.2.
--Tipado
--Indicar el tipo y el resultado de las siguientes expresiones:
--1. 5 :: Int
--2. 2.0 :: Fractional
--3. (5, 2.0) :: (Fractional t1, Num t)
--4. 5 + 2.0 :: Fractional
--5. minBound
--6. minBound && True
--7. succ :: Enum a => a -> a
--8. succ False :: Bool -> True
--9. succ True :: Bool -> False
--10. succ []
--11. succ ’a’
--12. read "5"
--13. read "5" :: Int
--14. let xs = [1,2,3] in xs
--15. Dado xs = [1,2,3] en un archivo, xs


















