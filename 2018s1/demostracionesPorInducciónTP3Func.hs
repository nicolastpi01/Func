--                                                   TP 3 - NICOLÁS MATÍAS GARCÍA
--                                                    DEMOSTRACIONES INDUCTIVAS

countFrom :: Int -> Int -> [Int]
countFrom 0 = []
countFrom n = n : countFrom (n-1)

1) factorial x = product (countFrom x)

-- Demostración por inducción en x

Caso base x = 0

factorial 0 = 1

product (CountFrom 0) = product [] = 1

Caso Inductivo x = h + 1

HI) factorial h = product (countFrom h) 
TI) factorial (h+1) = product (countFrom (h+1))

factorial (h+1) 
                               = (def factorial)
(h+1) * factorial h 
                               = (HI)
(h+1) * product (countFrom h)
                               = (def product)
product ((h+1) * countFrom h)
                               = (def countFrom)
product (countFrom (h+1))

--                 QED

-----------------------------------------------------------------------------------

2) length = sum . map (const 1)
                                      = ppio de extensionalidad
length xs = (sum . map (const 1)) xs
                                      = (def (.))
length xs = sum (map (const 1) xs)

-- demuestro por inducción estructural en xs

Caso base xs = []

length [] 
          = (def length)
0

sum (map (const 1) [])
                        = (def map)
sum []
                        = (def sum)
0

Caso inductivo xs = (h:hs)

HI) length hs = sum (map (const 1) hs)
TI) length (h:hs) = sum (map (const 1) (h:hs))

sum (map (const 1) (h:hs))
                                    = (def map)
sum (const 1 h : map (const 1) hs)
                                    = (def const)
sum (1 : map (const 1) hs)
                                    = (def sum)
1 + sum (map (const 1) hs)
                                    = HI
1 + length hs
                                    = (def length)
length (h:hs) 

--                QED

---------------------------------------------------------------------------


3) elem y = any (==y)

elem :: Eq a => a -> [a] -> Bool
elem y [] = False
elem y (x:xs) = y == x || elem y xs

elem y = any (==y)
                    = ppio extensionalidad

elem y xs = any (==y) xs

-- Demostración por inducción estructural en xs


Caso base, xs = []

elem y []
          = (def elem)
False

any (y==) []
            = (def any)
False

Caso inductivo, xs = (h:hs)

HI) elem y hs = any (==y) hs
TI) elem y (h:hs) = any (==y) (h:hs)


elem y (h:hs) = any (==y) (h:hs)
                                = def any
h == y || any (==y) hs
                                = HI
h == y || elem y hs
                                = def elem
elem y (h:hs)

--                QED


---------------------------------------------------------------------------


4) all f = and . (map f)
                              = ppio extensionalidad
all f xs = (and . (map f)) xs
                              = def (.)
all f xs = and (map f xs)

-- Demostración por inducción estructural en xs

Caso base xs = []

all f []
         = def all
True

and (map f [])
                = def map
and []
                = def and
True

Caso inductivo, xs = (h:hs)

HI) all f hs = and (map f hs)
TI) all f (h:hs) = and (map f (h:hs))

all f (h:hs) = and (map f (h:hs))
                                  = def map
and (f h : map f hs)
                                  = def and
f h && and (map f hs)
                                  = HI
f h && all f hs
                                  = def all

all f (h:hs)

--                   QED

-----------------------------------------------------------------------------------

5) (map f) . (map g) = map (f . g)
                                          = ppio extensionalidad
((map f) . (map g)) xs = map (f . g) xs
                                          = def (.)
map f (map g xs) = map (f . g) xs


-- Demostración por inducción estructural en xs

Caso base xs = []

map f (map g [])
                 = def map
map f []
                 = def map
[]

map (f . g) []
                 = def map
[]


Caso inductivo, xs = (h:hs)

HI) map f (map g hs) = map (f . g) hs
TI) map f (map g (h:hs)) = map (f . g) (h:hs)

map f (map g (h:hs))
                            = def map
map f (g h : map g hs)
                            = def map
f (g h) : map f (map g hs)  
                            = HI
f (g h) : map (f . g) hs
                            = def (.)
(f . g) h : map (f . g) hs
                            = def map

map (f . g) (h:hs)

--                    QED


----------------------------------------------------------------------------------------------

6) length = length . reverse
                                   = ppio extensionalidad
length xs = (length . reverse) xs
                                   = def (.)
length xs = length (reverse xs)

-- Demostración por inducción estructural en xs

Caso base xs = []

length []
          = def length
0

length (reverse [])
                    = def reverse
length []
                    = def length
0

Caso inductivo, xs = (h:hs)

HI) length hs = length (reverse hs)
TI) length (h:hs) = length (reverse (h:hs))

length (reverse (h:hs))
                                           = def reverse
length (reverse hs ++ [h])
                                           = lema 2)
length (reverse hs) + length [h]
                                           = def length
length (reverse hs) + length (x:[])
                                           = def length
length (reverse hs) + 1 + length []
                                           = def length
length (reverse hs) + 1 + 0
                                           = sumas
length (reverse hs) + 1
                                           = HI
length hs + 1
                                           = conmutatividad de la suma
1 + length hs
                                           = def length
length (h:hs)

--                           QED
                                                             

--1) lema : reverse (xs ++ ys) = reverse ys ++ reverse xs
--2) lema : length (xs ++ ys) = length xs + length ys

--reverse :: [a] -> [a]
--reverse [] = []
--reverse (x:xs) = reverse xs ++ [x]

--length :: [a] -> Int
--length [] = 0
--length (x:xs) = 1 + length xs

--(++) :: [a] -> [a] -> [a]
--(++) [] ys = ys
--(++) (x:xs) ys = x : (++) xs ys


-------------------------------------------------------------------------------------------------------------


7) length = length . map f
                                 = ppio extensionalidad

length xs = (length . map f) xs
                                 = def (.)

length xs = length (map f xs)

-- Demuestro por inducción estructural sobre listas en xs

Caso base, xs = []

length []
          = def length
0

length (map f [])
                   = def map
length []
                   = def length
0

Caso inductivo, xs = (h:hs)

HI) length hs = length (map f hs)
TI) length (h:hs) = length (map f (h:hs))


length (map f (h:hs))
                         = def map
length (f h : map f hs)
                         = def length
1 + length (map f hs)
                         = HI
1 + length hs
                         = def length
length (h:hs)


--                          QED


------------------------------------------------------------------------------------


8) flip (curry f) = curry (f . swap)
                                          = ppio extensionalidad
flip (curry f) x = curry (f . swap) x
                                          = ppio extensionalidad
flip (curry f) x y = curry (f . swap) x y

curry (f . swap) x y
                      = def curry
(f . swap) (x,y)
                      = def (.)
f (swap (x,y))
                      = def swap
f (y,x)

flip (curry f) x y
                    = def flip
curry f y x
                    = def curry
f (y,x)

f (y,x) = f (y,x)

-- LLegue a una expresión equivalente, queda demostrada la proposición para todo par de valores x e y.                                        


--------------------------------------------------------------------------------------


9) mirrorT . mirrorT = id
                             = ppio extensionalidad
(mirrorT - mirrorT) t = id t
                             = def (.)
mirrorT (mirrorT t) = id t

-- Demuestro por inducción estructural en t

Caso base, t = EmptyT

mirrorT (mirrorT t) = id t

id EmptyT
          = def id
EmptyT

mirrorT (mirrorT EmptyT)
                         = def mirrorT
mirrorT EmptyT
                         = def mirrorT
EmptyT

Caso inductivo, t = (NodeT x t1 t2)

HI) 1) mirrorT (mirrorT t1) = id t1
HI) 2) mirrorT (mirrorT t2) = id t2
TI) mirrorT (mirrorT (NodeT x t1 t2)) = id (NodeT x t1 t2)

mirrorT (mirrorT (NodeT x t1 t2))                = id (NodeT x t1 t2)
                                                       
mirrorT (mirrorT (NodeT x t1 t2))
                                                 = def mirrorT
mirrorT (NodeT x (mirrorT t2) (mirrorT t1))
                                                 = def mirrorT
NodeT x mirrorT(mirrorT t1) mirrorT(mirrorT t2)
                                                 = HI 1) y 2)
NodeT x (id t1) (id t2)
                                                 = def id
NodeT x t1 t2
                                                 = def id
id (NodeT x t1 t2) 


--                         QED

---------------------------------------------------------------------------------------------------  


10) sumT . mapT (const 1) = sizeT
                                     = ppio extensionalidad
(sumT . mapT (const 1)) t = sizeT t
                                     = def (.)
sumT(mapT (const 1) t) = sizeT t

-- Demuestro por inducción estructural en t

Caso base, t = EmptyT

sumT(mapT (const 1) EmptyT)
                            = def mapT
sumT EmptyT
                            = def sumT
0

sizeT EmptyT
              = def sizeT
0

Caso inductivo, t = (NodeT x t1 t2)

HI)
1) sumT(mapT (const 1) t1) = sizeT t1
2) sumT(mapT (const 1) t2) = sizeT t2

TI) sumT(mapT (const 1) (NodeT x t1 t2)) = sizeT (NodeT x t1 t2) 

sumT(mapT (const 1) (NodeT x t1 t2))
                                                                  = def mapT
sumT (NodeT (const 1 x) (mapT (const 1 t1)) (mapT (const 1 t2)))
                                                                  = def sumT
(const 1 x) + sumT (mapT (const 1 t1)) + sumT (mapT (const 1 t2))
                                                                  = HI
(const 1 x) + (sizeT t1) + (sizeT t2)
                                                                  = def const
1 + (size t1) + (size t2)
                                                                  = def sizeT
sizeT (NodeT x t1 t2)

--                           QED


--------------------------------------------------------------------------------------

11) sizeT = sizeT . mirrorT
                               = ppio extensionalidad
sizeT t = (sizeT . mirrorT) t
                               = def (.)
sizeT t = sizeT (mirrorT t)

-- Demuestro por inducción estructural en t

Caso base, t = EmptyT

sizeT EmptyT
             = def sizeT
0

sizeT (mirrorT EmptyT)
                        = def mirrorT
sizeT EmptyT
                        = def sizeT
0


Caso inductivo, t = (NodeT x t1 t2)

HI)
1) sizeT t1 = sizeT (mirrorT t1)
2) sizeT t2 = sizeT (mirrorT t2)
TI) sizeT (NodeT x t1 t2) = sizeT (mirrorT (NodeT x t1 t2)) 

sizeT (mirrorT (NodeT x t1 t2))
                                               = def mirrorT
sizeT (NodeT x (mirrorT t2) (mirrorT t1))
                                               = def sizeT
1 + (sizeT (mirrorT t2)) + (sizeT (mirrorT t1))
                                               = HI 1) 2)
1 + (sizeT t2) + (sizeT t1)
                                               = conmutatividad suma
1 + (sizeT t1) + (sizeT t2)
                                               = def sizeT
sizeT (NodeT x t1 t2)

--                       QED

-------------------------------------------------------------------------------

12) allT f = andT . (mapT f)
                                = ppio extensionalidad
allT f t = (andT . (mapT f)) t
                                = def (.)
allT f t = andT (mapT f t)

-- Demostración por inducción estructural en t

Caso base, t = EmptyT

allT f EmptyT
              = def allT
True

andT (mapT f EmptyT)
                     = def mapT
andT EmptyT
                     = def andT
True

Caso inductivo, t = (NodeT x t1 t2)

HI)
1) allT f t1 = andT (mapT f t1)
2) allT f t2 = andT (mapT f t2)
TI) allT f (NodeT x t1 t2) = andT (mapT f (NodeT x t1 t2))

andT (mapT f (NodeT x t1 t2))
                                               = def mapT
andT (NodeT (f x) (mapT f t1) (mapT f t2))
                                               = def andT
f x && (andT(mapT f t1)) && (andT(mapT f t2))
                                               = HI 1) 2)
f x && (allT f t1) && (allT f t2)
                                               = def allT
allT f (NodeT x t1 t2)

--                    QED

--------------------------------------------------------------------------------------

13) elemT e = anyT (==e)
                           = ppio extensionalidad
elemT e t = anyT (==e) t

-- Demuestro por inducción estructural en t

Caso base, t = EmptyT

elemT e EmptyT
                = def elemT
False

anyT (==e) t
             = def anyT
False

Caso inductivo, t = (NodeT x t1 t2)

HI)
1) elemT e t1 = anyT (==e) t1
2) elemT e t2 = anyT (==e) t2
TI) elemT e (NodeT x t1 t2) = anyT (==e) (NodeT x t1 t2)

anyT (==e) (NodeT x t1 t2)
                                               = def anyT
(==e) x || (anyT (==e) t1) || (anyT (==e) t2)
                                               = HI 1) 2)
x==e || (elemT e t1) || (elemT e t2)
                                               = def elemT
elemT e (NodeT x t1 t2)

--                     QED


----------------------------------------------------------------------------------------- 


14) countBy p = length . filter p
                                        = ppio extensionalidad
countBy p xs = (length . filter p) xs
                                        = def (.)
countBy p xs = length (filter p xs)

-- Demuestro por inducción estructural en xs

Caso base, xs = []

countBy p []
               = def countBy
0

length (filter p [])
                      = def filter
length []
                      = def length
0

Caso inductivo, xs = (h:hs)

HI) countBy p hs = length (filter p hs)
TI) countBy p (h:hs) = length (filter p (h:hs))

countBy p (h:hs) = length (filter p (h:hs))

length (filter p (h:hs))
                          = def filter
if p h then length (x : filter p hs) else length (filter p hs)

--  dos casos: p x == True; p x == False

1) p x == False

length (filter p hs)
                          = HI
countBy p hs
                          = def countBy
countBy p (h:hs)

2) p x == True

length (x : filter p hs)
                           = def length
1 + length (filter p hs)
                           = HI
1 + countBy p hs
                           = def countBy
countBy p (h:hs)


--                QED


--filter :: (a -> Bool) -> [a] -> [a]
--filter p [] = []
--filter p (x:xs) = 
--  if p x 
--     then x : filter p xs 
--     else filter p xs

--countBy :: (a -> Bool) -> [a] -> Int
--countBy p  [] = 0
--countBy p (x:xs) = 
--  if p x then 1 + countBy p xs else countBy p xs


------------------------------------------------------------------------------------------

15) concatMap f = concat . map f
                                       = ppio extensionalidad
concatMap f xs = (concat . map f) xs
                                       = def (.)
concatMap f xs = concat (map f xs)

-- Demostración por inducción estructural en xs

Caso base, xs = []

concatMap f []
                 = def concatMap
[]

concat (map f [])
                   = def map
concat []
                   = def concat
[]

Caso inductivo, xs = (h:hs)
HI) concatMap f hs = concat (map f hs)
TI) concatMap f (h:hs) = concat (map f (h:hs))

concat (map f (h:hs))
                            = def map
concat (f h : map f hs)
                            = def concat
(f h) ++ concat (map f hs)
                            = HI
(f h) ++ (concatMap f hs)
                            = def concatMap
concatMap f (h:hs)

--                 QED

------------------------------------------------------------------------

16) map f (xs ++ ys) = map f xs ++ map f ys

-- Demostración por inducción estructural en xs, ys.

Caso base, xs = [], ys = []

map f ([] ++ [])
                  = def (++)
map f []
                  = def map
[]

map f [] ++ map f []
                     = def map 
[] ++ []
                     = def (++)
[]

Caso inductivo, xs = (h:hs).

HI) map f (hs ++ ys) = map f hs ++ map f ys
TI) map f ((h:hs) ++ ys) = map f (h:hs) ++ map f ys

map f (h:hs) ++ map f ys
                                  = def map
f h : map f hs ++ map f ys
                                  = def (++)
f h : (++) (map f hs) (map f ys)
                                  = def (++)
f h : map f hs ++ map f ys
                                  = HI
f h : map f (hs ++ ys)
                                  = def map
map f (h : (hs ++ ys))
                                  = def (++)
map f ((h:hs) ++ ys)



--                  QED

---------------------------------------------------------------------------------


17) map f . concat = concat . map (map f)
                                                = ppio extensionalidad
(map f . concat) xs = (concat . map (map f)) xs
                                                = def (.)
map f (concat xs) = concat (map (map f) xs)

-- Demuestro por inducción estructural en xs

Caso base, xs = []

map f (concat [])
                  = def concat
map f []
                  = def map
[]


concat (map (map f) [])
                         = def map
concat []
                         = def concat
[]

Caso inductivo, xs = (h:hs)

HI) map f (concat hs) = concat (map (map f) hs)
TI) map f (concat (h:hs)) = concat (map (map f) (h:hs))

concat (map (map f) (h:hs))
                                      = def map
concat ((map f) h : map (map f) hs)
                                      = def concat
(map f) h ++ concat (map (map f) hs))
                                      = HI
(map f) h ++ map f (concat hs)
                                      = def map
map f (h ++ (concat hs))  
                                      = def concat
map f (concat (h:hs))

--                   QED

-------------------------------------------------------------------------

18) reverse (xs ++ ys) = reverse ys ++ reverse xs

-- Demuestro por inducción estructural en xs

Caso base, xs = [] 

reverse ([] ++ ys)
                    = def (++)
reverse ys


reverse ys ++ reverse []
                         = def reverse
reverse ys ++ []
                         = conmutatividad (++)
[] ++ reverse ys
                         = def (++)
reverse ys

Caso inductivo, xs = (h:hs)

HI) reverse (hs ++ ys) = reverse ys ++ reverse hs
TI) reverse ((h:hs) ++ ys) = reverse ys ++ reverse (h:hs)

reverse ys ++ reverse (h:hs)
                                = def reverse
reverse ys ++ reverse hs ++ [h]
                                = HI
reverse (hs ++ ys) ++ [h]
                                = def reverse
reverse (h : (hs ++ ys))
                                = def (++)
reverse ((++) (h:hs) ys)
                                = def (++)
reverse ((h:hs) ++ ys)


--                 QED



--reverse :: [a] -> [a]
--reverse [] = []
--reverse (x:xs) = reverse xs ++ [x]

--length :: [a] -> Int
--length [] = 0
--length (x:xs) = 1 + length xs

--(++) :: [a] -> [a] -> [a]
--(++) [] ys = ys
--(++) (x:xs) ys = x : (++) xs ys

-----------------------------------------------------------------------------------------


19) length (zipWith f xs ys) = min (length xs) (length ys)

-- Demuestro por inducción estructural en xs .

Caso base, xs = [], ys = []

length (zipWith f [] [])
                         = def zipWith
length []
                         = def length
0

min (length []) (length [])
                             = def length (2 veces)
min 0 0
                             = def min
0

Caso base especial "inductivo a medias", xs = (h:hs), ys = []


length (zipWith f (h:hs) [])
                             = def zipWith
length []
                             = def length
0

min (length (h:hs)) (length [])
                                 = def length (2 veces)
min (1 + length hs) 0
                                 = def min
0


Caso base especial "inductivo a medias", xs = [], ys = (d:ds)

length (zipWith f [] (d:ds))
                               = def zipWith
length []
                               = def length
0

min (length []) (length (d:ds))
                                 = def length (2 veces)
min 0 (1 + length ds)
                                 = def min
0

Caso inductivo, xs = (h:hs), ys = (d:ds) .

HI) length (zipWith f hs ds) = min (length hs) (length ds)
TI) length (zipWith f (h:hs) (d:ds)) = min (length (h:hs)) (length (d:ds))

length (zipWith f (h:hs) (d:ds))
                                    = def zipWith
length (f h d : zipWith f hs ds)
                                    = def length
1 + length (zipWith f hs ds)
                                    = HI
1 + min (length hs) (length ds)
                                    = def min
min (length (h:hs)) (length (d:ds))


--                       QED

--zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
--zipWith f [] _  = []
--zipWith f _  [] = []
--zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

--length :: [a] -> Int
--length [] = 0
--length (x:xs) = 1 + length xs

--(++) :: [a] -> [a] -> [a]
--(++) [] ys = ys
--(++) (x:xs) ys = x : (++) xs ys


-----------------------------------------------------------------------------------------------------


20) sum (xs ++ ys) = sum (zipWith (+) xs ys) (dar contraejemplo)

 "Un contraejemplo son dos listas con distinta longitud" Ej: 

xs = [1,2,3]

ys = [1,2,3,4]

sum ([1,2,3] ++ [1,2,3,4])
                                    = def (++)
sum (1 : (++) [2,3] [1,2,3,4])
                                    = def sum
1 + sum ((++) [2,3] [1,2,3,4])
                                    = def (++)
1 + sum (2 : (++) [3] [1,2,3,4])
                                    = def sum
1 + 2 + sum ((++) [3] [1,2,3,4])
                                    = def (++)
1 + 2 + sum(3 : (++) [] [1,2,3,4])
                                    = def sum
1 + 2 + 3 + sum((++) [] [1,2,3,4])
                                    = def (++)
1 + 2 + 3 + sum [1,2,3,4]
                                    = def sum
1 + 2 + 3 + 1 + sum [2,3,4]
                                    = def sum
1 + 2 + 3 + 1 + 2 + sum [3,4]
                                    = def sum
1 + 2 + 3 + 1 + 2 + 3 + sum [4]
                                    = def sum
1 + 2 + 3 + 1 + 2 + 3 + 4 + sum []
                                    = def sum
1 + 2 + 3 + 1 + 2 + 3 + 4 + 0
                                    = sumas
16

sum (zipWith (+) [1,2,3] [1,2,3,4])
                                          = def zipWith
sum (1 + 1 : zipWith (+) [2,3] [2,3,4])
                                          = def sum
2 + sum (zipWith (+) [2,3] [2,3,4])
                                          = def zipWith
2 + sum (2 + 2 : zipWith (+) [3] [3,4])
                                          = def sum
2 + 4 + sum (zipWith (+) [3] [3,4])
                                          = def zipWith
2 + 4 + sum (3 + 3 : zipWith (+) [] [4])
                                          = def sum
2 + 4 + 6 + sum (zipWith (+) [] [4])
                                          = def zipWith
2 + 4 + 6 + sum []
                                          = def sum
2 + 4 + 6 + 0
                                          = sumas
12    


* No se demuestra la prop con contraejemplo;  xs = [1,2,3] ; ys = [1,2,3,4] .

--zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
--zipWith f [] _  = []
--zipWith f _  [] = []
--zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys


-------------------------------------------------------------------------------------------------------------------------


24) takewhile p xs ++ dropwhile p xs = xs

-- Demuestro por inducción estructural sobre xs

Caso base, xs = [] 

takewhile p [] ++ dropWhile p []
                                 = def takeWhile
[] ++ dropWhile p []
                                 = def dropWhile
[] ++ []
                                 = def (++)
[]

Caso inductivo, xs = (h:hs)

HI) takewhile p hs ++ dropwhile p hs = hs
TI) takewhile p (h:hs) ++ dropwhile p (h:hs) = h:hs

takewhile p (h:hs) ++ dropwhile p (h:hs)
                                           = def takeWhile
Dos casos:
1) p h = True
2) p h = False

1) rama then...
(h : takeWhile p hs) ++ dropWhile p (h:hs)
                                               = def (++)
h : (++) (takeWhile p hs) (dropWhile p (h:hs))
                                               = def dropWhile
h : (++) (takeWhile p hs) (dropWhile p hs)
                                               = def (++)
h : takeWhile p hs ++ dropWhile p hs
                                               = HI
h:hs

2) rama else...

* No se puede resolver de forma inductiva, por eso si p h es False...

takeWhile p (h:hs) ++ dropWhile p (h:hs)
                                                   = def takeWhile
[] ++ dropWhile p (h:hs)
                                                   = def dropWhile
[] ++ (h:hs)
                                                   = def (++)
(h:hs)

--                         QED


--(++) :: [a] -> [a] -> [a]
--(++) [] ys = ys
--(++) (x:xs) ys = x : (++) xs ys

--takeWhile :: (a -> Bool) -> [a] -> [a]
--takeWhile p [] = []
--takeWhile p (x:xs) = if p x then x : takeWhile p xs else []

--dropWhile :: (a -> Bool) -> [a] -> [a]
--dropWhile p [] = []
--dropWhile p (x:xs) = if p x then dropWhile p xs else x:xs



--------------------------------------------------------------------------------------------------------

                                      -- TP 3 -(el posta) Demostraciones inductivas


5) any p = or . map p
                            = ppio extensionalidad
any p xs = (or . map p) xs
                            = def (.)
any p xs = or (map p xs)

-- Demuestro por inducción estructural en xs

Caso base, xs = []

any p []
          = def any
False

or (map p [])
                = def map
or []
                = def or
False

Caso inductivo, xs = (h:hs)

HI) any p hs = or (map p hs)
TI) any p (h:hs) = or (map p (h:hs))


or (map p (h:hs))
                      = def map
or (p h : map p hs)
                      = def or
p h || or (map p hs)
                      = HI
p h || any p hs
                      = def any
any p (h:hs) 

--               QED

--any, all :: (a -> Bool) -> [a] -> Bool
--any p [] = False
--any p (x:xs) = p x || any p xs

--and, or :: [Bool] -> Bool
--and [] = True
--and (x:xs) = x && and xs

--or [] = False
--or (x:xs) = x || or xs

--map :: (a -> b) -> [a] -> [b]
--map f [] = []
--map f (x:xs) = f x : map f xs

----------------------------------------------------------------------------------------------

7) length (xs ++ ys) = lenght xs + lenght ys

-- Demuestro por inducción estructural en xs .

Caso base, xs = []

length ([] ++ ys)
                     = def (++)
length ys
             

lenght [] + lenght ys
                        = def lenght
0 + lenght ys
                        = sumas
lenght ys


Caso inductivo, xs = (h:hs) 

HI) length (hs ++ ys) = lenght hs + lenght ys
TI) length ((h:hs) ++ ys) = lenght (h:hs) + lenght ys

length ((h:hs) ++ ys)
                            = def (++)
length (h : (++) hs ds)
                            = def length
1 + length ((++) hs ds)
                            = def (++)
1 + length (hs ++ ds)
                            = HI
1 + length hs + length ys
                            = def length
length (h:hs) + length ys

--                QED

--(++) :: [a] -> [a] -> [a]
--(++) [] ys = ys
--(++) (x:xs) ys = x : (++) xs ys

--length :: [a] -> Int
--length [] = 0
--length (x:xs) = 1 + length xs

--------------------------------------------------------------------------------------


10) subset xs ys = all (‘elem‘ ys) xs

-- Demuestro por inducción estructural en xs

Caso base, xs = []

subset [] ys
              = def subset
True

all (`elem` ys) []
                    = def all
True

Caso inductivo, xs = (h:hs)

HI) subset hs ys = all (‘elem‘ ys) hs
TI) subset (h:hs) ys = all (‘elem‘ ys) (h:hs)

all (‘elem‘ ys) (h:hs)
                                   = def all
h ‘elem‘ ys && all (‘elem‘ ys) hs
                                   = HI
h ‘elem‘ ys && subset hs ys
                                   = def subset
subset (h:hs) ys

--                QED

--subset :: Eq a => [a] -> [a] -> Bool
--subset [] ys = True
--subset (x:xs) ys = elem x ys && subset xs ys

--all p [] = True
--all p (x:xs) = p x && all p xs

--elem, notElem :: Eq a => a -> [a] -> Bool
--elem x [] = False
--elem a (x:xs) = x == a || elem a xs


----------------------------------------------------------------------------

11) concat = concatMap id
                              = ppio extensionalidad
concat xss = concatMap id xss

-- Demuestro por inducción estructural

Caso base, xss = []

concat []
           = def concat
[]

concatMap id []
                = def concatMap
[]

Caso inductivo, xss = (hs:hss)

HI) concat hss = concatMap id hss
TI) concat (hs:hss) = concatMap id (hs:hss)

concatMap id (hs:hss)
                           = def concatMap
id hs ++ concatMap id hss
                           = HI
id hs ++ concat hss
                           = def id
hs ++ concat hss
                           = def concat
concat (hs:hss)

--                QED


--concat :: [[a]] -> [a]
--concat [] = []
--concat (x:xs) = x ++ concat xs

--concatMap :: (a -> [b]) -> [a] -> [b]
--concatMap f [] = []
--concatMap f (x:xs) = f x ++ concatMap f xs

--id :: a -> a
--id x = x

---------------------------------------------------------------------------

13) replicate n x = applyN n (x:) []

-- Demuestro por inducción estructural en n

Caso base, n = 0 .

replicate 0 x
               = def replicate
[]

applyN 0 (x:) []
                 = def applyN
id []
                 = def id
[]


Caso inductivo, n = m+1

HI) replicate m x = applyN m (x:) []
TI) replicate (m+1) x = applyN (m+1) (x:) [] 

replicate (m+1) x
                              = def replicate
x : replicate (m+1-1) x
                              = sumas
x : replicate m x
                              = HI
x : ( applyN m (x:) [] )
                              = def (.)
((x:) . applyN m (x:)) []
                              = def applyN
applyN (m+1) (x:) []


--                    QED


--replicate :: Int -> a -> [a]
--replicate 0 a = []
--replicate n a = a : replicate (n-1) a

--applyN :: Int -> (a -> a) -> a -> a
--applyN 0 f = id
--applyN n f = f . applyN (n-1) f

------------------------------------------------------------------------------------

14) snoc xs y = xs ++ [y]

-- Demuestro por inducción estructural en xs

Caso base, xs = []

snoc [] y
          = def snoc
[y]

[] ++ [y]
          = def (++)
[y]

Caso inductivo, xs = (h:hs)

HI) snoc hs y = hs ++ [y]
TI) snoc (h:hs) y = (h:hs) ++ [y]

snoc (h:hs) y
                 = def snoc
h : snoc hs y
                 = HI
h : hs ++ [y]
                 = def (:)
(h:hs) ++ [y]

--          QED


--(++) :: [a] -> [a] -> [a]
--(++) [] ys = ys
--(++) (x:xs) ys = x : (++) xs ys

--snoc :: [a] -> a -> [a]
--snoc [] y = [y]
--snoc (x:xs) y = x : snoc xs y


------------------------------------------------------------------------------

15) notElem = (.) not . elem
                                       = ppio extensionalidad
notElem x = (.) (not . elem) x
                                       = ppio extensionalidad
notElem x xs = (.) (not . elem) x xs
                                       = def (.)
notElem x xs = (.) not (elem x) xs
                                       = def (.)
notElem x xs = not (elem x xs) 

-- Demuestro por inducción estructural en xs

Caso base, xs = []

notElem x []
              = def notElem
True

not (elem x [])
                 = def elem
not False
                 = def not
True

Caso inductivo, xs = (h:hs)

HI) notElem x hs = not (elem x hs)
TI) notElem x (h:hs) = not (elem x (h:hs))

not (elem x (h:hs))
                           = def elem
not (x == h || elem x hs)
                           = def (==)
not (False || elem x hs)
                           = der (||)
not (elem x hs)
                           = HI
notElem x hs
                           = def (&&)
x /= h && notElem x hs
                           = def notElem
notElem x (h:hs)


--               QED  



--elem x [] = False
--elem a (x:xs) = x == a || elem a xs

--notElem :: Eq a => a -> [a] -> Bool
--notElem y [] = True
--notElem y (x:xs) = y /= x && notElem y xs

----------------------------------------------------------------------------------------------

26) last = head . reverse
                               = ppio extensionalidad
last xs = (head . reverse) xs
                               = def (.)
last xs = head (reverse xs)

-- Demostracción por inducción estructural en xs

Caso base, xs = (s:[])

last (s:[])
            = def last
s

head (reverse (s:[]))
                         = def reverse
head (reverse [] ++ [s])
                         = def reverse
head ([] ++ [s])
                         = def (++)
head [s]
                         = def head
s

Caso inductivo, xs = (h:hs)

HI) last hs = head (reverse hs)
TI) last (h:hs) = head (reverse (h:hs))

head (reverse (h:hs))
                            = def reverse
head (reverse hs ++ [h])
                            = 



ERRORERRORERRORERRORERRORERRORERRORERRORERRORERRORERRORERRORERRORERRORERRORERRORERRORERRORERROR

--1) lema : reverse (xs ++ ys) = reverse ys ++ reverse xs
--2) lema : length (xs ++ ys) = length xs + length ys

--last :: [a] -> a
--last (x:[]) = x
--last (x:xs) = last xs

--head :: [a] -> a
--head (x:xs) = x

--reverse :: [a] -> [a]
--reverse [] = []
--reverse (x:xs) = reverse xs ++ [x]

--(++) :: [a] -> [a] -> [a]
--(++) [] ys = ys
--(++) (x:xs) ys = x : (++) xs ys



----------------------------------------------------------------------------------------------

28) filter p (xs ++ ys) = filter p xs ++ filter p ys

-- Demuestro por inducción estructural en xs

Caso base, xs = []

filter p ([] ++ ys)
                        = def (++)
filter p ys

filter p [] ++ filter p ys
                            = def filter
[] ++ filter p ys
                            = def (++)
filter p ys

Caso inductivo, xs = (h:hs)

HI) filter p (hs ++ ys) = filter p hs ++ filter p ys
TI) filter p ((h:hs) ++ ys) = filter p (h:hs) ++ filter p ys

filter p (h:hs) ++ filter p ys
                                 = def filter
Dos casos:
1) p h True
2) p h False

1)
if p h then h : filter p hs else filter p hs ++ filter p ys
                                                             = 1)
h : filter p hs ++ filter p ys
                                                             = HI
h : filter p (hs ++ ys)
                                                             = def (++)
h: filter p (hs ++ ys)
                                                             = def filter
filter p ((h:hs) ++ ys)

2)
if p h then h : filter p hs else filter p hs ++ filter p ys
                                                             = 2
filter p hs ++ filter p ys
                                                             = HI
filter p (hs ++ ys)
                                                             = def filter
filter p ((h:hs) ++ ys)


--                                 QED

--filter :: (a -> Bool) -> [a] -> [a]
--filter p [] = []
--filter p (x:xs) = if p x then x : filter p xs else filter p xs

--(++) :: [a] -> [a] -> [a]
--(++) [] ys = ys
--(++) (x:xs) ys = x : (++) xs ys



-------------------------------------------------------------------------------------------------------------

35) (!!) n = head . drop n
                               = ppio extensionalidad
(!!) xs n = (head . drop n) xs
                               = def (.)
(!!) xs n = head (drop n xs)

-- Demuestro por inducción estructural en n .

Caso base, n = 0; xs = (h:hs)  

(!!) (h:hs) 0
           = def (!!)
h

head (drop 0 (h:hs))
                     = def drop
head (h:hs)
                     = def head
h

Caso inductivo, n = m+1; xs = (h:hs)
HI) (!!) hs m = head (drop m hs)
TI) (!!) (h:hs) (m+1) = head (drop (m+1) (h:hs))  

head (drop (m+1) (h:hs))
                          = def drop
head (drop m (h:hs))
                          = def head

ERRORERRORERRORERRORERRORERRORERRORERRORERRORERRORERRORERRORERRORERRORERRORERRORERRORERROR

--head :: [a] -> a
--head (x:xs) = x

--(!!) :: [a] -> Int -> a
--(!!) (x:xs) 0 = x 
--(!!) (x:xs) n = xs !! (n-1)

--drop :: Int -> [a] -> [a]
--drop 0 xs = xs
--drop n [] = []
--drop n (x:xs) = drop (n-1) xs


---------------------------------------------------------------------------------------------------------------------------

6) (map f) . (map g) = map (f . g)
                                         = ppio extensionalidad
((map f) . (map g)) xs = map (f . g) xs
                                         = def (.)
map f (map g xs) = map (f . g) xs

-- Demuestro por inducción estructural en xs

Caso base, xs = []

map f (map g [])
                      = def map
map f []
                      = def map
[]

map (f . g) []
                      = def map
[]

Caso inductivo, xs = (h:hs)

HI) map f (map g hs) = map (f . g) hs
TI) map f (map g (h:hs)) = map (f . g) (h:hs)

map (f . g) (h:hs)
                             = def map
(f . g) h : map (f . g) hs
                             = HI
(f . g) h : map f (map g hs)
                             = def (.)
f (g h) : map f (map g hs)
                             = def map
map f ((g h):(map g hs))
                             = def map
map f (map g (h:hs))

--             QED

--map :: (a -> b) -> [a] -> [b]
--map f [] = []
--map f (x:xs) = f x : map f xs



----------------------------------------------------------------------------------------------

29) filter p (filter q xs) = filter (\y -> p y && q y) xs

-- Demuestro por inducción estructural en xs

Caso base, xs = []

filter p (filter q [])
                         = def filter
filter p []
                         = def filter
[]

filter (\y -> p y && q y) []
                              = def filter
[]

Caso inductivo, xs = (h:hs)

HI) filter p (filter q hs) = filter (\y -> p y && q y) hs
TI) filter p (filter q (h:hs)) = filter (\y -> p y && q y) (h:hs)

filter p (filter q (h:hs))
                             = def filter
Dos casos:
1) q h = True
2) q h = False

1)
filter p (h :  filter q hs)
                             = def filter
Dos casos:
1) p h = True
2) p h = False

-- CASO 1 (q h = True && p h = True)

h : filter p (filter q hs)
                                  = HI
h : filter (\y -> p y && q y) hs
                                  = def filter
filter (\y -> p y && q y) (h:hs)



-- CASO 2 (q h = False ...)

filter p (filter q (h:hs))
                             = def filter
filter p (filter q hs)
                                 = HI
filter (\y -> p y && q y) hs
                                 = def filter
filter (\y -> p y && q y) (h:hs)


-- CASO 3 (q h = True && p h = False)

filter p (filter q (h:hs))
                             = def filter
filter p (h : filter q hs)
                             = def filter
filter p (filter q hs)
                             = HI
filter (\y -> p y && q y) hs
                                 = def filter
filter (\y -> p y && q y) (h:hs)

--                QED

--filter :: (a -> Bool) -> [a] -> [a]
--filter p [] = []
--filter p (x:xs) = if p x then x : filter p xs else filter p xs


----------------------------------------------------------------------------------

30) filter p . map f = map f . filter (p . f)
                                                     = ppio extensionalidad
(filter p . map f) xs = (map f . filter (p . f)) xs
                                                     = def (.)
filter p (map f xs) = map f (filter (p . f) xs)

--Demuestro por inducción estructural en xs .

Caso base, xs = []

filter p (map f [])
                      = def map
filter p []
                      = def filter
[]

map f (filter (p . f) [])
                            = def filter
map f []
                            = def map
[]

Caso inductivo, xs = (h:hs)

HI) filter p (map f hs) = map f (filter (p . f) hs)
TI) filter p (map f (h:hs)) = map f (filter (p . f) (h:hs))

map f (filter (p . f) (h:hs))
                               = def filter
--2 casos
1) (p . f) h = True
2) (p . f) h = False

Desarrollo caso 1)

map f (if (p . f) h then h : filter (p . f) hs else filter (p . f) hs)     
                                                                        = ya que (p . f) h = True
map f (h : filter (p . f) hs)
                                                                        = def map
f h : map f (filter (p . f) hs)
                                                                        = HI
f h : filter p (map f hs)
                                                                        = def filter
filter p (map f (h:hs))

Desarrollo caso 2)

map f (if (p . f) h then h : filter (p . f) hs else filter (p . f) hs)     
                                                                        = ya que (p . f) h = False
map f (filter (p . f) hs)
                                                                        = HI
filter p (map f hs)
                                                                        = def filter
filter p (map f (h:hs))

--                                QED


--filter :: (a -> Bool) -> [a] -> [a]
--filter p [] = []
--filter p (x:xs) = if p x then x : filter p xs else filter p xs

--map :: (a -> b) -> [a] -> [b]
--map f [] = []
--map f (x:xs) = f x : map f xs


--------------------------------------------------------------------------------------------

-- DEMOSTRACIÓN LEMAS

2) lema : length (xs ++ ys) = length xs + length ys

-- Demuestro por inducción estructural en xs

Caso base, xs = []

length ([] ++ ys)
                   = def (++)
length ys

length [] + length ys
                       = def length
0 + length ys
                       = neutro de la suma
length ys

Caso inductivo, xs = (h:hs)

HI) length (hs ++ ys) = length hs + length ys
TI) length ((h:hs) ++ ys) = length (h:hs) + length ys

length ((h:hs) ++ ys)
                            = def (++)
length (h : (++) hs ys)
                            = def length
1 + length (hs ++ ys)
                            = HI 
1 + length hs + length ys
                            = def length
length (h:hs) + length ys


--                QED











