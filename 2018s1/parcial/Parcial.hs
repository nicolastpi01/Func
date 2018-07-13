

--                                                            PARCIAL


-- MAPA

-- Representaremos un mapa donde se van presentando objetos. Existen cofres en los finales de caminos, puntos que no poseen objetos, y -- bifurcaciones con objetos, y dos caminos posibles. Además existen direcciones para recorrer el mapa: Left (izquierda), Right (derecha),
-- Straight (centro). Un camino [Dir] en este mapa representa un camino desde la raiz del mapa hasta un cofre. Las definiciones de tipo son las
-- sig:

data Dir = Left' | Right' | Straight deriving (Show)

data Mapa a = Cofre [a] | Nada (Mapa a) | Bifurcacion [a] (Mapa a) (Mapa a) deriving (Show)

-- 1) Definir las sig. funciones usando recursión explicita sobre la est. de mapa (se pueden ver ejemplos en el anexo):

objects :: Mapa a -> [a]
objects (Cofre xs) = xs
objects (Nada m) = objects m
objects (Bifurcacion xs m1 m2) = xs ++ (objects m1) ++ (objects m2)

mapM' :: (a -> b) -> Mapa a -> Mapa b
mapM' f (Cofre xs) = Cofre (map f xs)
mapM' f (Nada m) = Nada (mapM' f m)
mapM' f (Bifurcacion xs m1 m2) = Bifurcacion (map f xs) (mapM' f m1) (mapM' f m2)

hasObjectAt :: (a -> Bool) -> Mapa a -> [Dir] -> Bool
hasObjectAt f (Cofre xs) [] = any f xs
hasObjectAt f (Nada m) [] = False
hasObjectAt f (Nada m) (Straight:ds) = hasObjectAt f m ds
hasObjectAt f (Bifurcacion xs m1 m2) [] = False
hasObjectAt f (Bifurcacion xs m1 m2) (Straight:ds) = any f xs
hasObjectAt f (Bifurcacion xs m1 m2) (Left':ds) = hasObjectAt f m1 ds
hasObjectAt f (Bifurcacion xs m1 m2) (Right':ds) = hasObjectAt f m2 ds


longestPath :: Mapa a -> [Dir]
longestPath (Cofre xs) = []
longestPath (Nada m) = Straight : (longestPath m)
longestPath (Bifurcacion xs m1 m2) = let (zs,ys) = (longestPath m1,longestPath m2)
                                     in if length zs >= length ys then Left' : zs
                                                                  else Right' : ys

objectsOfLongestPath :: Mapa a -> [a]
objectsOfLongestPath (Cofre xs) = xs
objectsOfLongestPath (Nada m) = objectsOfLongestPath m
objectsOfLongestPath (Bifurcacion xs m1 m2) = let (zs,ys) = (longestPath m1,longestPath m2)
                                              in if length zs >= length ys then xs ++ (objectsOfLongestPath m1)
                                                                           else xs ++ (objectsOfLongestPath m2)


allPaths :: Mapa a -> [[Dir]]
allPaths (Cofre xs) = [[]]
allPaths (Nada m) = map (Straight:) (allPaths m)
allPaths (Bifurcacion xs m1 m2) = (map (Left':) (allPaths m1)) ++
                                  (map (Right':) (allPaths m2))



-- 2) Dar tipo y definir foldM y recMm una versión de fold y recursión primitiva, respectivamente para la estrcutura de Mapa.

foldM :: ([a] -> b) -> (b -> b) -> ([a] -> b -> b -> b) -> Mapa a -> b
foldM f z g (Cofre xs) = f xs
foldM f z g (Nada m) = z (foldM f z g m)
foldM f z g (Bifurcacion xs m1 m2) = g xs (foldM f z g m1) (foldM f z g m2)


recM :: ([a] -> b) -> (b -> Mapa a -> b) -> ([a] -> Mapa a -> Mapa a -> b  -> b -> b) -> Mapa a -> b
recM f z g (Cofre xs) = f xs
recM f z g (Nada m) = z (recM f z g m) m
recM f z g (Bifurcacion xs m1 m2) = g xs m1 m2 (recM f z g m1) (recM f z g m2)


-- 3) Definir las funciones del primer punto usando foldM y recM, según corresponda.

objects' :: Mapa a -> [a]
objects' = foldM id id (\ xs r1 r2 -> xs ++ r1 ++ r2)

mapM'' :: (a -> b) -> Mapa a -> Mapa b
mapM'' f = foldM (\ xs -> Cofre (map f xs)) (\ r -> Nada r) (\ xs r1 r2 -> Bifurcacion (map f xs) r1 r2)


hasObjectAt' :: (a -> Bool) -> Mapa a -> [Dir] -> Bool
hasObjectAt' f m ds = foldM g h j m ds
                      where g xs [] = any f xs
                            h r [] = False
                            h r (Straight:xs) = r xs
                            j xs r1 r2 [] = False
                            j xs r1 r2 (Straight:ys) = any f xs
                            j xs r1 r2 (Left':ys) = r1 ys
                            j xs r1 r2 (Right':ys) = r2 ys



longestPath' :: Mapa a -> [Dir]
longestPath' = foldM (const []) (\r -> Straight : r) g
                                             where g xs r1 r2 = let (zs,ys) = (r1,r2)
                                                                in if length zs >= length ys then Left' : r1
                                                                                             else Right' : r2


objectsOfLongestPath' :: Mapa a -> [a]
objectsOfLongestPath' = recM id (\ r m -> r) g
                              where g xs m1 m2 r1 r2 = let (zs,ys) = (longestPath m1, longestPath m2)
                                                 in if length zs >= length ys then xs ++ r1
                                                                              else xs ++ r2


allPaths' :: Mapa a -> [[Dir]]
allPaths' = foldM (\ xs -> [[]]) (\r -> map (Straight:) r) (\ xs r1 r2 -> (map (Left':) r1) ++ (map (Right':) r2))



-----------------------------------------------------------------------------------------------------------------------------------

--                                                DEMOSTRACIONES

-- 4) demostrar las siguientes equivalencias usando las funciones definidas en el punto 1

-- a) length . objects = countObjects
--                                                 = ppio ext
-- (length . objects) m = countObjects m
--                                                 = def (.)
-- length (objects m) = countObjects m

-- Demuestro por induccion estructural en m para todo m

-- Caso base, m = (Cofre xs)

-- length (objects (Cofre xs))
--                             = def objects
-- length xs

-- countObjects (Cofre xs)
--                         = def countObjects
-- length xs


-- Caso inductivo, (m = Nada m1) || (m = Nada m2) || (m = Bifurcacion xs m1 m2)

-- HI)
-- 2) length (objects m2) = countObjects m2
-- 3) length (objects m1) = countObjects m1
-- TI)
-- length (objects (Bifurcacion xs m1 m2)) = countObjects (Bifurcacion xs m1 m2)

-- length (objects (Bifurcacion xs m1 m2))
--                                                       = def objects
-- length (xs ++ (objects m1) ++ (objects m2))
--                                                       = lema0
-- length xs + length (objects m1) + length (objects m2)
--                                                       = HI) 1) y 2)
-- length xs + countObjects m1 + countObjects m2
--                                                       = countObjects (al revés)
-- countObjects (Bifurcacion xs m1 m2)

--                          QED


-- lema0 Demostración

-- length (xs ++ ys) = length xs + length ys

-- Demuestro por inducción estrutural en xs

-- Caso base, xs = []

-- length ([] ++ ys)
--                    = def (++)
-- length ys

-- length [] + length ys
--                        = def (++)
-- 0 + length ys
--                        = aritmetica
-- length ys

-- Caso inductivo, xs = (h:hs)

-- HI) length (hs ++ ys) = length hs + length ys
-- TI) length ((h:hs) ++ ys) = length (h:hs) + length ys

-- length ((h:hs) ++ ys)
--                          = def length
-- 1 + length (hs ++ ys)
--                           = HI)
-- 1 + length hs + length ys
--                           = def length (al revés)
-- length (h:hs) + length ys

--                      QED




-- Suponer ya demostrada la sig. propiedad = Para todo x, as, bs : elem x (as ++ bs) = elem x as || elem x bs
-- b) elem x . objects = hasObjectAt (==x)

--elem x . objects = hasObjectAt (==x)
--                                              = ppio extensionalidad
--(elem x . objects) m = hasObjectAt (==x) m
--                                              = ppio extensionalidad
--(elem x . objects) m = hasObjectAt (==x) m xs
--                                              = def (.)
--elem x (objects m) = hasObjectAt (==x) m xs

--Demuestro por inducción estructural en m para todo m

--Caso base, m = (Cofre xs)

--elem x (objects (Cofre xs))
--                                 = def objects
--elem x xs

--hasObjectAt (==x) (Cofre xs) xs
--                                 = def hasObjectAt



-- c) length . map f . map g . objects = countObjects . mapM (f . g)




--hasObjectAt :: (a -> Bool) -> Mapa a -> [Dir] -> Bool
--hasObjectAt f (Cofre xs) [] = any f xs
--hasObjectAt f (Nada m) [] = False
--hasObjectAt f (Nada m) (Straight:ds) = hasObjectAt f m ds
--hasObjectAt f (Bifurcacion xs m1 m2) [] = False
--hasObjectAt f (Bifurcacion xs m1 m2) (Straight:ds) = any f xs
--hasObjectAt f (Bifurcacion xs m1 m2) (Left':ds) = hasObjectAt f m1 ds
--hasObjectAt f (Bifurcacion xs m1 m2) (Right':ds) = hasObjectAt f m2 ds

--countObjects (Cofre xs) = length xs
--countObjects (Nada m) = countObjects m
--countObjects (Bifurcacion xs m1 m2) = length xs + countObjects m1 + countObjects m2

--objects :: Mapa a -> [a]
--objects (Cofre xs) = xs
--objects (Nada m) = objects m
--objects (Bifurcacion xs m1 m2) = xs ++ (objects m1) ++ (objects m2)

--mapM' :: (a -> b) -> Mapa a -> Mapa b
--mapM' f (Cofre xs) = Cofre (map f xs)
--mapM' f (Nada m) = Nada (mapM' f m)
--mapM' f (Bifurcacion xs m1 m2) = Bifurcacion (map f xs) (mapM' f m1) (mapM' f m2)
