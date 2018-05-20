
-- 1. Árboles binarios


data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving (Show)
data Persona = Persona String Int Int deriving (Show)

sumT :: Tree Int -> Int
sumT EmptyT = 0
sumT (NodeT x t1 t2) = x + (sumT t1) + (sumT t2) 

sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT x t1 t2) = 1 + (sizeT t1) + (sizeT t2)

mapDoubleT :: Tree Int -> Tree Int
mapDoubleT EmptyT = EmptyT
mapDoubleT (NodeT x t1 t2) = NodeT (x*2) (mapDoubleT t1) (mapDoubleT t2)

mapLengthT :: Tree String -> Tree Int
mapLengthT EmptyT = EmptyT
mapLengthT (NodeT x t1 t2) = NodeT (length x) (mapLengthT t1) (mapLengthT t2)

elemT :: Eq a => a -> Tree a -> Bool
elemT x EmptyT = False
elemT x (NodeT y t1 t2) = x == y || (elemT x t1) || (elemT x t2)

occurrsT :: Eq a => a -> Tree a -> Int
occurrsT y EmptyT = 0
occurrsT y (NodeT x t1 t2) = if y == x then 1 + (occurrsT y t1) + (occurrsT y t2) else (occurrsT y t1) + (occurrsT y t2)

--Necesario para resolver averageT
age :: Persona -> Int
age (Persona name age dni) = age

--Necesario para resolver averageT
mapT :: (a -> b) -> Tree a -> Tree b
mapT f EmptyT = EmptyT
mapT f (NodeT x t1 t2) = NodeT (f x) (mapT f t1) (mapT f t2) 

averageT :: Tree Persona -> Int
averageT t = sumT(mapT age t) `div` (sizeT t)


countLeaves :: Tree a -> Int
countLeaves EmptyT = 0
countLeaves (NodeT x EmptyT EmptyT) = 1
countLeaves (NodeT x t1 t2) = (countLeaves t1) + (countLeaves t2)  

leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT x EmptyT EmptyT) = [x]
leaves (NodeT x t1 t2) = (leaves t1) ++ (leaves t2)


heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT x EmptyT EmptyT) = 1 
heightT (NodeT x t1 t2) = 1 + max (heightT t1) (heightT t2)

countNoLeaves, countNoLeaves' :: Tree a -> Int
countNoLeaves EmptyT = 0
countNoLeaves (NodeT x EmptyT EmptyT) = 0
countNoLeaves (NodeT x t1 t2) = 1 + (countNoLeaves t1) + (countNoLeaves t2)


countNoLeaves' t = (sizeT t) - (countLeaves t)


mirrorT :: Tree a -> Tree a
mirrorT EmptyT = EmptyT
mirrorT (NodeT x t1 t2) = NodeT x (mirrorT t2) (mirrorT t1)

listInOrder :: Tree a -> [a]
listInOrder EmptyT = []
listInOrder (NodeT x t1 t2) = listInOrder t1 ++ [x] ++ listInOrder t2

listPreOrder :: Tree a -> [a]
listPreOrder EmptyT = []
listPreOrder (NodeT x t1 t2) = x : listPreOrder t1 ++ listPreOrder t2

listPosOrder :: Tree a -> [a]
listPosOrder EmptyT = []
listPosOrder (NodeT x t1 t2) = (listPosOrder t1) ++ (listPosOrder t2) ++ [x]

concatT :: Num a => Tree [a] -> [a]
concatT EmptyT = []
concatT (NodeT x t1 t2) = (concatT t1) ++ x ++ (concatT t2) 

levelN :: Int -> Tree a -> [a]
levelN _ EmptyT = []
levelN 0 (NodeT x t1 t2) = [x]
levelN n (NodeT x t1 t2) = (levelN (n-1) t1) ++ (levelN (n-1) t2)  

--Necesario para listPerLevel
specialZipWith :: (a -> a -> a) -> [a] -> [a] -> [a]
specialZipWith f [] [] = []
specialZipWith f xs [] = xs
specialZipWith f [] ys = ys
specialZipWith f (x:xs) (y:ys) = f x y : specialZipWith f xs ys 

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT x EmptyT EmptyT) = [[x]]
listPerLevel (NodeT x EmptyT t2) = [x] : listPerLevel t2
listPerLevel (NodeT x t1 EmptyT) = [x] : listPerLevel t1
listPerLevel (NodeT x t1 t2) = [x] : specialZipWith (++) (listPerLevel t1) (listPerLevel t2)



widthT :: Ord a => Tree a -> Int
widthT = length . maximum . listPerLevel  



leftBranches :: Tree a -> [a]
leftBranches EmptyT = []
leftBranches (NodeT x EmptyT _) = []
leftBranches (NodeT x t1@(NodeT y _ _) _) = y : leftBranches t1   


longestBranch :: Tree a -> [a]
longestBranch EmptyT = []
longestBranch (NodeT x EmptyT t@(NodeT x1 t1 t2)) = x : (longestBranch t) 
longestBranch (NodeT x t@(NodeT x1 t1 t2) EmptyT) = x : (longestBranch t)
longestBranch (NodeT x t1 t2) = let (xss,yss) = (longestBranch t1,longestBranch t2) 
                                                     in if length xss >= length yss then x:xss else x:yss



allPaths :: Tree a -> [[a]]
allPaths EmptyT = []
allPaths (NodeT x EmptyT EmptyT) = [[x]]
allPaths (NodeT x t1 EmptyT) = map (x:) (allPaths t1)
allPaths (NodeT x EmptyT t2) = map (x:) (allPaths t2)
allPaths (NodeT x t1 t2) = map (x:) (allPaths t1) ++ map (x:) (allPaths t2)
 

-----------------------------------------------------------------------------------------------------------------------------------------------



data Mapa = Cofre Objeto | Bifurcacion Objeto Mapa Mapa deriving (Show)

data Dir = Izq | Der deriving (Show)

data Objeto = Tesoro | Chatarra deriving (Show)



hayTesoro :: Mapa -> Bool
hayTesoro (Cofre Tesoro) = True
hayTesoro (Cofre Chatarra) = False
hayTesoro (Bifurcacion Tesoro m1 m2) = True 
hayTesoro (Bifurcacion Chatarra m1 m2) = (hayTesoro m1) || (hayTesoro m2) 


hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] (Cofre Tesoro) = True
hayTesoroEn [] (Cofre Chatarra) = False
hayTesoroEn _ (Cofre Tesoro) = True
hayTesoroEn _ (Cofre Chatarra) = False
hayTesoroEn [] (Bifurcacion Chatarra m1 m2) = False
hayTesoroEn (Izq:xs) (Bifurcacion Chatarra m1 m2) = hayTesoroEn xs m1
hayTesoroEn (Der:xs) (Bifurcacion Chatarra m1 m2) = hayTesoroEn xs m2
hayTesoroEn _ (Bifurcacion Tesoro m1 m2) = True



-- Precond: Hay un camino hacia el tesoro en el mapa y es único
caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Cofre Tesoro) = []
caminoAlTesoro (Cofre Chatarra) = []
caminoAlTesoro (Bifurcacion Chatarra (Cofre Tesoro) _) = [Izq]
caminoAlTesoro (Bifurcacion Chatarra _ (Cofre Tesoro)) = [Der] 
caminoAlTesoro (Bifurcacion Tesoro _ _) = []
caminoAlTesoro (Bifurcacion Chatarra m1 m2) = if length (caminoAlTesoro m1) >= 1 then Izq : (caminoAlTesoro m1) else Der : (caminoAlTesoro m2) 


caminoRamaMasLarga :: Mapa -> [Dir]
caminoRamaMasLarga (Cofre Tesoro) = []
caminoRamaMasLarga (Cofre Chatarra) = []
caminoRamaMasLarga (Bifurcacion Chatarra (Cofre Chatarra) (Cofre Tesoro)) = [Der]
caminoRamaMasLarga (Bifurcacion Chatarra (Cofre Tesoro) (Cofre Chatarra)) = [Izq]
caminoRamaMasLarga (Bifurcacion _ m1 m2) = if length (caminoRamaMasLarga m1) >= length (caminoRamaMasLarga m2) 
                                                         then Izq : (caminoRamaMasLarga m1) else Der : (caminoRamaMasLarga m2) 




tesorosPerLevel :: Mapa -> [[Objeto]]
tesorosPerLevel (Cofre Chatarra) = []
tesorosPerLevel (Cofre obj) = [[obj]]
tesorosPerLevel (Bifurcacion Chatarra (Cofre Chatarra) (Cofre Chatarra)) = []
tesorosPerLevel (Bifurcacion Chatarra (Cofre Chatarra) m2) = tesorosPerLevel m2
tesorosPerLevel (Bifurcacion Chatarra m1 (Cofre Chatarra)) = tesorosPerLevel m1
tesorosPerLevel (Bifurcacion Chatarra m1 m2) = specialZipWith (++) (tesorosPerLevel m1) (tesorosPerLevel m2)
tesorosPerLevel (Bifurcacion obj m1 m2) = [obj] : specialZipWith (++) (tesorosPerLevel m1) (tesorosPerLevel m2)




todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Cofre Tesoro) = []
todosLosCaminos (Cofre Chatarra) = []
todosLosCaminos (Bifurcacion _ (Cofre Tesoro) (Cofre Tesoro)) = [[Izq],[Der]]
todosLosCaminos (Bifurcacion _ (Cofre Chatarra) (Cofre Chatarra)) = [[Izq],[Der]]
todosLosCaminos (Bifurcacion _ (Cofre Chatarra) (Cofre Tesoro)) = [[Izq],[Der]]
todosLosCaminos (Bifurcacion _ (Cofre Tesoro) (Cofre Chatarra)) = [[Izq],[Der]]
todosLosCaminos (Bifurcacion _ m1 m2) = map (Izq:) (todosLosCaminos m1) ++ map (Der:) (todosLosCaminos m2)




---------------------------------------------------------------------------------------------------------------------------------------------

-- 3. Expresiones aritméticas


data Exp = Constante Int | ConsExpUnaria OpUnaria Exp | ConsExpBinaria OpBinaria Exp Exp deriving (Show)

data OpUnaria = Neg deriving (Show)

data OpBinaria = Suma | Resta | Mult | Div deriving (Show)



eval :: Exp -> Int
eval (Constante x) = x
eval (ConsExpUnaria Neg exp) = -(eval exp)
eval (ConsExpBinaria Suma exp0 exp1) = (eval exp0) + (eval exp1)
eval (ConsExpBinaria Resta exp0 exp1) = (eval exp0) - (eval exp1)
eval (ConsExpBinaria Mult exp0 exp1) = (eval exp0) * (eval exp1)
eval (ConsExpBinaria Div exp0 exp1) = (eval exp0) `div` (eval exp1)


simplify :: Exp -> Exp  
simplify (ConsExpBinaria Suma (Constante 0) exp1) = simplify exp1
simplify (ConsExpBinaria Suma exp0 (Constante 0)) = simplify exp0
simplify (ConsExpBinaria Resta (Constante 0) exp1) = ConsExpUnaria Neg (simplify exp1)
simplify (ConsExpBinaria Mult exp0 (Constante 1)) = simplify exp0
simplify (ConsExpBinaria Mult (Constante 1) exp1) = simplify exp1
simplify (ConsExpBinaria Mult exp0 (Constante 0)) = Constante 0
simplify (ConsExpBinaria Mult (Constante 0) exp1) = Constante 0
simplify (ConsExpBinaria Div exp0 (Constante 1)) = exp0
simplify (ConsExpBinaria Div (Constante 0) exp1) = if eval exp1 /= 0 then Constante 0 else error "la división por cero no esta definida"
simplify exp = exp


































