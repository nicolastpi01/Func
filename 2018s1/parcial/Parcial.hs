

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
hasObjectAt _ _ _ = error "el camino no existe en el mapa"


longestPath :: Mapa a -> [Dir]
longestPath (Cofre xs) = []
longestPath (Nada m) = Straight : (longestPath m)
longestPath (Bifurcacion xs m1 m2) = let (xs,ys) = (longestPath m1,longestPath m2)
                                     in if length xs >= length ys then Left' : (longestPath m1) 
                                                                  else Right' : (longestPath m2)

objectsOfLongestPath :: Mapa a -> [a]
objectsOfLongestPath (Cofre xs) = xs
objectsOfLongestPath (Nada m) = objectsOfLongestPath m 
objectsOfLongestPath (Bifurcacion xs m1 m2) = let (xs,ys) = (longestPath m1,longestPath m2)
                                              in if length xs >= length ys then objectsOfLongestPath m1  
                                                                           else objectsOfLongestPath m2



allPaths :: Mapa a -> [[Dir]]
allPaths (Cofre xs) = [[]]
allPaths (Nada m) = map (Straight:) (allPaths m)
allPaths (Bifurcacion xs m1 m2) = (map (Left':) (allPaths m1)) ++ 
                                  (map (Right':) (allPaths m2))

                            

-- 2) Dar tipo y definir foldM y recMm una versión de fold y recursión primitiva, respectivamente para la estrcutura de Mapa.

foldM :: ([a] -> b) -> (b -> b) -> ([a] -> b -> b -> b) -> Mapa a -> b
foldM f z q (Cofre xs) = f xs
foldM f z q (Nada m) = z (foldM f z q m)
foldM f z q (Bifurcacion xs m1 m2) = q xs (foldM f z q m1) (foldM f z q m2)
 

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
                            g xs ys = error "el camino no existe en el mapa"
                            h r [] = False
                            h r (Straight:xs) = r xs
                            h r ds = error "el camino no existe en el mapa"
                            j xs r1 r2 [] = False
                            j xs r1 r2 (Straight:ys) = any f xs
                            j xs r1 r2 (Left':ys) = r1 ys
                            j xs r1 r2 (Right':ys) = r2 ys
                            j xs r1 r2 _ = error "el camino no existe en el mapa"



longestPath' :: Mapa a -> [Dir]
longestPath' = foldM (const []) (\r -> Straight : r) g
                                             where g xs r1 r2 = let (xs,ys) = (r1,r2)
                                                                in if length xs >= length ys then Left' : r1 
                                                                                             else Right' : r2

objectsOfLongestPath' :: Mapa a -> [a]
objectsOfLongestPath' = foldM id id g
                              where g xs r1 r2 = let (xs,ys) = (r1,r2)
                                                 in if length xs >= length ys then r1  
                                                                              else r2


allPaths' :: Mapa a -> [[Dir]]
allPaths' = foldM (\ xs -> [[]]) (\r -> map (Straight:) r) (\ xs r1 r2 -> (map (Left':) r1) ++ (map (Right':) r2))



-----------------------------------------------------------------------------------------------------------------------------------

--                                                DEMOSTRACIONES

-- 4) demostrar las siguientes equivalencias usando las funciones definidas en el punto 1

-- a) length . objects = countObjects
--                                                 = ppio ext
--(length . objects) t = countObjects t
--                                                 = def (.)
--length (objects t) = countObjects t

--Demuestro por induccion estructural en t para todo t

--Caso base, t = (Cofre xs)

--length (objects (Cofre xs))
--                             = def objects
--length xs

--countObjects (Cofre xs)
--                         = def countObjects
--length xs


--data Mapa a = Cofre [a] | Nada (Mapa a) | Bifurcacion [a] (Mapa a) (Mapa a) deriving (Show)
--Caso inductivo, t =   


--objects :: Mapa a -> [a]
--objects (Cofre xs) = xs
--objects (Nada m) = objects m
--objects (Bifurcacion xs m1 m2) = xs ++ (objects m1) ++ (objects m2)


-- b) elem x . objects = hasObjectAt (==x)
-- Suponer ya demostrada la sig. propiedad = Para todo x, as, bs : elem x (as ++ bs) = elem x as || elem x bs

-- c) length . map f . map g . objects = countObjects . mapM (f . g)

--countObjects (Cofre xs) = length xs
--countObjects (Nada m) = countObjects m
--countObjects (Bifurcacion xs m1 m2) = length xs + countObjects m1 + countObjects m2


 




