-- 						Ejercicio Integrador (Nicolás García)


data ThreeT a = Leaf a | Branch a (ThreeT a) (ThreeT a) (ThreeT a) deriving (Show)

--1. Definir las siguientes funciones usando recursión explícita sobre la estructura de ThreeT:

sizeTT :: ThreeT a -> Int
sizeTT (Leaf x) = 1
sizeTT (Branch x t1 t2 t3) = 1 + (sizeTT t1) + (sizeTT t2) + (sizeTT t3)

sumTT :: ThreeT Int -> Int
sumTT (Leaf x) = x
sumTT (Branch x t1 t2 t3) = x + (sumTT t1) + (sumTT t2) + (sumTT t3)

leavesTT :: ThreeT a -> [a]
leavesTT (Leaf x) = [x]
leavesTT (Branch x t1 t2 t3) = [x] ++ (leavesTT t1) ++ (leavesTT t2) ++ (leavesTT t3)

mapTT :: (a -> b) -> ThreeT a -> ThreeT b
mapTT f (Leaf x) = Leaf (f x)
mapTT f (Branch x t1 t2 t3) = Branch (f x) (mapTT f t1) (mapTT f t2) (mapTT f t3)

maxTT :: Ord a => ThreeT a -> a
maxTT (Leaf x) = x
maxTT (Branch x t1 t2 t3) = max x (max (maxTT t1) (max (maxTT t2) (maxTT t3)))

findTT :: Eq a => (a -> Bool) -> ThreeT (a,b) -> Maybe b
findTT f (Leaf (x,y)) = if f x then Just y else Nothing 
findTT f (Branch (x,y) t1 t2 t3) = if f x then Just y else g (findTT f t1) (findTT f t2) (findTT f t3)
                                   where 
                                   g (Just x) m2 m3 = Just x
                                   g m1 (Just x) m3 = Just x
                                   g m1 m2 (Just x) = Just x
                                   g m1 m2 m3 = Nothing


levelNTT :: Int -> ThreeT a -> [a]
levelNTT n (Leaf x) = if n == 0 then [x] else []
levelNTT 0 (Branch x t1 t2 t3) = [x]
levelNTT n (Branch x t1 t2 t3) = (levelNTT (n-1) t1) ++ (levelNTT (n-1) t2) ++ (levelNTT (n-1) t3)  


listPerLevelTT :: ThreeT a -> [[a]]
listPerLevelTT (Leaf x) = [[x]]
listPerLevelTT (Branch x t1 t2 t3) = [x] : specialZipWith (++) (specialZipWith (++) (listPerLevelTT t1) (listPerLevelTT t2)) (listPerLevelTT t3)
                                     

--Necesario para listPerLevelTT
specialZipWith :: (a -> a -> a) -> [a] -> [a] -> [a]
specialZipWith f [] [] = []
specialZipWith f xs [] = xs
specialZipWith f [] ys = ys
specialZipWith f (x:xs) (y:ys) = f x y : specialZipWith f xs ys 


------------------------------------------------------------------------------------------------------------------------------------------

-- 2. Dar una definición de fold (llamada foldTT) en base a la estructura del tipo ThreeT.

foldTT :: (a -> b -> b -> b -> b) -> (a -> b) -> ThreeT a -> b
foldTT f g (Leaf x) = g x
foldTT f g (Branch x t1 t2 t3) = f x (foldTT f g t1) (foldTT f g t2) (foldTT f g t3) 


-- 3. Definir las funciones del primer punto usando la definición dada de fold para ThreeT.

sizeTT' :: ThreeT a -> Int
sizeTT' = foldTT (\ x r1 r2 r3 -> 1 + r1 + r2 + r3) (\_ -> 1)

sumTT' :: ThreeT Int -> Int
sumTT' = foldTT (\ x r1 r2 r3 -> x + r1 + r2 + r3) id

leavesTT' :: ThreeT a -> [a]
leavesTT' = foldTT (\ x r1 r2 r3 -> [x] ++ r1 ++ r2 ++ r3) (\ y -> [y])

mapTT' :: (a -> b) -> ThreeT a -> ThreeT b
mapTT' f = foldTT (\ x r1 r2 r3 -> Branch (f x) r1 r2 r3) (\ y -> Leaf (f y))

maxTT' :: Ord a => ThreeT a -> a
maxTT' = foldTT (\ x r1 r2 r3 -> max (max (max x r1) r2) r3) id

findTT' :: Eq a => (a -> Bool) -> ThreeT (a,b) -> Maybe b
findTT' f = foldTT (\ (x,y) r1 r2 r3 -> if f x then Just y else g r1 r2 r3) (\ (x,y) -> if f x then Just y else Nothing) 
                                                          where g (Just x) m2 m3 = Just x
                                                                g m1 (Just x) m3 = Just x
                                                                g m1 m2 (Just x) = Just x
                                                                g m1 m2 m3 = Nothing 
                                                                                 

levelNTT' :: Int -> ThreeT a -> [a]
levelNTT' m t = foldTT g h t m
                 where g x r1 r2 r3 n = if n == 0
                                        then [x]
                                        else r1 (n-1) ++ r2 (n-1) ++ r3 (n-1)
                       h x n = if n == 0 
                               then [x] 
                               else []  


listPerLevelTT' :: ThreeT a -> [[a]]
listPerLevelTT' = foldTT (\ x r1 r2 r3 -> [x] : specialZipWith (++) (specialZipWith (++) r1 r2) r3) (\ x -> [[x]])

---------------------------------------------------------------------------------------------------------------------------------------


-- 4. Demostrar las siguientes equivalencias usando las funciones definidas en el punto 1.

-- a) sizeTT = sumTT . mapTT (const 1)
--                                        = ppio ext
-- sizeTT t = (sumTT . mapTT (const 1)) t
--                                        = def (.)
-- sizeTT t = sumTT (mapTT (const 1) t)

-- Demuestro por inducción estructural en t

-- Caso base, t = Leaf x

-- sizeTT (Leaf x)
--                  = def sizeTT
-- 1

-- sumTT (mapTT (const 1) (Leaf x))
--                                     = def mapTT
-- sumTT (Leaf ((const 1) x) )
--                                     = def const
-- sumTT (Leaf 1)
--                                     = def sumTT
-- 1

-- Caso inductivo, t = (Branch x t1 t2 t3)

-- HI)
-- 1) sizeTT t1 = sumTT (mapTT (const 1) t1)
-- 2) sizeTT t2 = sumTT (mapTT (const 1) t2)
-- 3) sizeTT t3 = sumTT (mapTT (const 1) t3)
-- TI)
-- sizeTT (Branch x t1 t2 t3) = sumTT (mapTT (const 1) (Branch x t1 t2 t3) )  

-- sumTT (mapTT (const 1) (Branch x t1 t2 t3))
--                                                                                                  = def mapTT
-- sumTT (Branch ((const 1) x) (mapTT (const 1) t1) (mapTT (const 1) t2) (mapTT (const 1) t3) )
--                                                                                                  = def const
-- sumTT (Branch 1 (mapTT (const 1) t1) (mapTT (const 1) t2) (mapTT (const 1) t3) )
--                                                                                                  = def sumTT
-- 1 + (sumTT (mapTT (const 1) t1) ) + (sumTT (mapTT (const 1) t2) ) + (sumTT (mapTT (const 1) t3) )
--                                                                                                  = HI 1) 2) 3)
-- 1 + sizeTT t1 + sizeTT t2 + sizeTT t3
--                                                                                                  = def sizeTT (al revés)
-- sizeTT (Branch x t1 t2 t3)

--                                           QED 


--b) sum . leavesTT = sumTT

-- sum . leavesTT = sumTT
--                                = ppio ext
-- (sum . leavesTT) t = sumTT t
--                                = def (.)
-- sum (leavesTT t) = sumTT t

-- Demuestro por inducción estructural en t

-- Caso base, t = (Leaf x)

-- sum (leavesTT (Leaf x))
--                          = def leavesTT
-- sum [x]
--                          = def sum
-- x

-- sumTT (Leaf x)
--                          = def sumTT
-- x

-- Caso inductivo, t = (Branch x t1 t2 t3)

-- HI)
-- 1) sum (leavesTT t1) = sumTT t1
-- 2) sum (leavesTT t2) = sumTT t2
-- 3) sum (leavesTT t3) = sumTT t3
-- TI)
-- sum (leavesTT (Branch x t1 t2 t3)) = sumTT (Branch x t1 t2 t3)

-- sumTT (Branch x t1 t2 t3)
--                                                                     = def sumTT
-- x + (sumTT t1) + (sumTT t2) + (sumTT t3)
--                                                                     = HI 1) 2) 3)
-- x + (sum (leavesTT t1)) + (sum (leavesTT t2)) + (sum (leavesTT t3)) 
--                                                                     = def sum (al revés)
-- sum (leavesTT (Branch x t1 t2 t3))


--                      INCOMPLETO


--c) sizeTT . mapTT f . mapTT g = sizeTT . mapTT (f . g)

-- sizeTT . mapTT f . mapTT g = sizeTT . mapTT (f . g)
--                                                              = ppio ext
-- (sizeTT . mapTT f . mapTT g) t = (sizeTT . mapTT (f . g)) t
--                                                              = def (.)
-- (sizeTT (mapTT f (mapTT g t))) = sizeTT (mapTT (f . g) t)

-- Demuestro por inducción estructural en t

-- Caso base, t = (Leaf x)

-- (sizeTT (mapTT f (mapTT g (Leaf x))))
--                                              = def mapTT
-- sizeTT (mapTT f (Leaf (g x)))
--                                              = def mapTT
-- sizeTT (Leaf (f (g x) ) )
--                                              = def sizeTT
-- 1

-- sizeTT (mapTT (f . g) (Leaf x))
--                                              = def mapTT
-- sizeTT (Leaf ((f . g) x))
--                                              = def sizeTT
-- 1

-- Caso inductivo, t = (Branch x t1 t2 t3)

-- HI)
-- 1) (sizeTT (mapTT f (mapTT g t1))) = sizeTT (mapTT (f . g) t1)
-- 2) (sizeTT (mapTT f (mapTT g t2))) = sizeTT (mapTT (f . g) t2)
-- 3) (sizeTT (mapTT f (mapTT g t3))) = sizeTT (mapTT (f . g) t3)

-- TI) (sizeTT (mapTT f (mapTT g (Branch x t1 t2 t3)))) = sizeTT (mapTT (f . g) (Branch x t1 t2 t3))

-- sizeTT (mapTT (f . g) (Branch x t1 t2 t3))
--                                                                                                         = def mapTT
-- sizeTT (Branch ((f . g) x) (mapTT (f . g) t1) (mapTT (f . g) t2) (mapTT (f . g) t3))
--                                                                                                         = def sizeTT
-- 1 + (sizeTT (mapTT (f . g) t1)) + (sizeTT (mapTT (f . g) t2)) + (sizeTT (mapTT (f . g) t3))
--                                                                                                         = HI 1) 2) 3)
-- 1 + (sizeTT (mapTT f (mapTT g t1))) + (sizeTT (mapTT f (mapTT g t2))) + (sizeTT (mapTT f (mapTT g t3)))
--                                                                                                         = def sizeTT (al revés)
-- (sizeTT (mapTT f (mapTT g (Branch x t1 t2 t3))))

--                                    QED 


-- d ) maximum . leavesTT = maxTT
--                                    = ppio ext
-- (maximum . leavesTT) t = maxTT t
--                                    = def (.)
-- maximum (leavesTT t) = maxTT t

-- Demuestro por inducción estructural en t

-- Caso base, t = (Leaf x)

-- maximum (leavesTT (Leaf x))
--                                 = def leavesTT
-- maximum [x]
--                                 = def maximum
-- x

-- maxTT (Leaf x)
--                                 = def maxTT
-- x

-- Caso inductivo, t = (Branch x t1 t2 t3)

-- HI)
-- 1) maximum (leavesTT t1) = maxTT t1
-- 2) maximum (leavesTT t2) = maxTT t2
-- 3) maximum (leavesTT t3) = maxTT t3
-- TI) maximum (leavesTT (Branch x t1 t2 t3)) = maxTT (Branch x t1 t2 t3)


-- maximum (leavesTT (Branch x t1 t2 t3))
--                                                                   = def leavesTT
--maximum ([x] ++ (leavesTT t1) ++ (leavesTT t2) ++ (leavesTT t3))
--                                                                   = def maximum
--x `max` maximum ((leavesTT t1) ++ (leavesTT t2) ++ (leavesTT t3))


--                                INCOMPLETO









 
