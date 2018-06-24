 
--                                                       PRÁCTICA 5 -- MONADAS


data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving (Show)

mapT :: (a -> b) -> Tree a -> Tree b
mapT f EmptyT = EmptyT
mapT f (NodeT x t1 t2) = NodeT (f x) (mapT f t1) (mapT f t2) 

foldT :: (a -> b -> b -> b) -> b -> Tree a -> b
foldT f g EmptyT = g
foldT f g (NodeT x t1 t2) = f x (foldT f g t1) (foldT f g t2)

class Semigroup a where
  (<>) :: a -> a -> a


--class Semigroup a => Monoid a where
--  mempty :: a
--  mappend :: a -> a -> a 
--  mconcat :: [a] -> a
--  mconcat = foldr mappend mempty

class Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a
  mconcat = foldr mappend mempty


--Donde se cumple que mempty es el neutro de mappend, y además mappend es asociativa (técni-
--camente <> y mappend son sinónimos).

--1. Definir instancias de Monoid para los siguientes tipos de datos:

newtype Sum = Sum Int

getSum (Sum x) = x

instance Monoid Sum where
  mempty = Sum 0
  mappend (Sum x) (Sum y) = Sum (x+y)

newtype Mult = Mult Int

getMult (Mult x) = x

instance Monoid Mult where
  mempty = Mult 1
  mappend (Mult x) (Mult y) = Mult (x*y)


newtype All = All Bool

getAll (All x) = x

instance Monoid All where
  mempty = All True
  mappend (All x) (All y) = All (x && y)


newtype Any = Any Bool

getAny (Any x) = x

instance Monoid Any where
  mempty = Any False
  mappend (Any x) (Any y) = Any (x || y)


newtype Endo a = Endo (a -> a)

appEndo (Endo f) = f


instance Monoid (Endo a) where
  mempty = Endo id 
  mappend (Endo f) (Endo g) = Endo (f . g)


instance Monoid [a] where
  mempty = [] 
  mappend xs ys = xs ++ ys


-------------------------------------------------------------------------------------------------------------------------------------

--2. Resolver las siguientes funciones utilizando esta typeclass:

sum, mult :: [Int] -> Int

sum = getSum . mconcat . map Sum 

mult = getMult . mconcat . map Mult

and', or' :: [Bool] -> Bool

and' = getAll . mconcat . map All

or' = getAny . mconcat . map Any

concat' :: [[a]] -> [a]
concat' = mconcat

id' :: a -> a
id' = appEndo (Endo (\x -> x))

twice :: (a -> a) -> a -> a
twice f = appEndo (Endo f `mappend` Endo f) 

--compose :: (b -> c) -> (a -> b) -> (a -> c) 
--compose g f = appEndo (Endo (f . g) )

concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f = mconcat . map f

--applyN :: Int -> (a -> a) -> a -> a
--applyN 0 f x = x
--applyN n f x = Endo f `mappend`(Endo (applyN (n-1) f x))

ntimes :: Monoid a => Int -> a -> a
ntimes 0 x = mempty
ntimes n x = x `mappend` ntimes (n-1) x

factorial' :: Int -> Int
factorial' 1 = 1
factorial' n = getMult ( Mult n `mappend` Mult (factorial' (n-1)) )

sumatoria :: Int -> Int
sumatoria 1 = 1
sumatoria n = getSum ( Sum n `mappend` Sum (sumatoria (n-1)) )

tconcat :: Monoid a => Tree a -> a
tconcat = foldT (\ x r1 r2 -> x `mappend` r1 `mappend` r2) mempty

sumT :: Tree Int -> Int
sumT = getSum . tconcat . mapT Sum

anyT :: (a -> Bool) -> Tree a -> Bool
anyT f = getAny . tconcat . mapT (Any . f) 

allT :: (a -> Bool) -> Tree a -> Bool
allT f = getAll . tconcat . mapT (All . f)

concatT :: Tree [a] -> [a]
concatT = tconcat

----------------------------------------------------------------------------------------------------------------------------------------

--2) Functors
--2.1)
--Ejemplos
--Indicar el resultado de las siguientes expresiones:
--1) fmap (replicate 3) [1,2,3,4] = [[1,1,1],[2,2,2],[3,3,3],[4,4,4]]
--2) fmap (replicate 3) (Just 4) = Just [4,4,4] 
--3. fmap (replicate 3) (Right "hola") = Right ["hola","hola","hola"]
--4. fmap (replicate 3) Nothing = Nothing
--5. fmap (replicate 3) (Left "foo") = Left "foo"


-----------------------------------------------------------------------------------------------------------------------------------------

--3) List
--3.1 Applicative Functor
-- 1. Dada la siguiente implementación de Applicative Functor para listas

--NO FUNCIONA EL FUNCTOR
--instance Applicative [] where
--pure x = [x]
--fs <*> xs = [f x | f <- fs, x <- xs]

--indicar el resultado de las siguientes expresiones

--[(*2),(+100)] <*> [1,2,3] = [102,104,106]
--[(+),(*)] <*> [1,2] <*> [3,4] = []
--(++) <$> ["hola","chau","gracias"] <*> ["?","!","."]
--filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11]




-------------------------------------------------------------------------------------------------------------------------------------------

--                                                           LIST MONAD

--3.2) List Monad
--a) Indicar el resultado de las siguientes expresiones:
--[3,4,5] >>= \x -> [x,-x] = [3,-3,4,-4,5,-5]  
--[] >>= \x -> ["bad","mad","rad"] = []
--[1,2,3] >>= \x -> [] = []
--[1,2,3] >>= (\x -> [x,x]) >>= (\y -> return [y,y*y]) = [[1,1],[1,1],[2,4],[2,4],[3,9],[3,9]]

--b) Definir las siguientes funciones utilizando la mónada de listas

productoCartesiano :: [a] -> [b] -> [(a, b)]
productoCartesiano xs ys = do
                           x <- xs
                           y <- ys
                           return (x,y) 


data Resultado = Cara | Seca deriving (Show)

tiradas :: Int -> [[Resultado]]
tiradas 0 = return []
tiradas n =  do
               xs <- tiradas (n-1)
               [Seca:xs, Cara:xs]


--(Desafío) Se recomienda usar filterM (ver Hoogle)
--powerset :: [a] -> [[a]]

---------------------------------------------------------------------------------------------------------------------------------









--6) Funciones Genéricas

-- Definir las siguientes funciones estándar sobre mónadas en general (ver ejemplos en Hoogle):

--class Functor m => Monad m where
--  return :: a -> m a
--  (>>=) :: m a -> (a -> m b) -> m b

-- PREGUNTAR
void :: Monad m => m a -> m ()
void mx = return ()

when :: Monad f => Bool -> f () -> f ()
when b mx = if b then mx else return ()


liftM :: Monad m => (a -> b) -> m a -> m b
liftM f mx = do
               x <- mx
               return (f x)


liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f mx my = do
                   x <- mx
                   y <- my
                   return (f x y) 


appM :: (Monad m) => m (a -> b) -> m a -> m b
appM mf mx = do
               f <- mf
               x <- mx
               return (f x)


join :: (Monad m) => m (m a) -> m a
join mmx = do
             mx <- mmx
             mx

sequence' :: Monad m => [m a] -> m [a]
sequence' [] = return []
sequence' (mx:mxs) = do
                      x  <- mx
                      xs <- sequence' mxs
                      return (x:xs)


sequence'_ :: Monad m => [m a] -> m ()
sequence'_ = foldr (>>) (return ())


mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f = sequence . map f


mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f = sequence_ . map f

-- NO FUNCA!!!
--filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
--filterM f xs = sequence . map f xs

forM :: (Monad m) => [a] -> (a -> m b) -> m [b]
forM xs f = mapM f xs


(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(>=>) f g x = f x >>= g
                    
forever :: Monad m => m a -> m b
forever mx = mx >> forever mx

(>>$) :: Monad m => m a -> m b -> m b                         
(>>$) mx my = do
                mx
                my
                
  
-- Que es aplicative? FALLA!!!                     
--zipWithM :: Applicative m => (a -> b -> m c) -> [a] -> [b] -> m [c]
--zipWithM f xs = sequence . zipWith f xs


--foldM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
--foldM f x = foldr (\ y r -> f x y >> r) (return ())


--foldM_ :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m ()
--foldM_ f x = foldr (\ y r -> f x y >> r) (return ())


replicateM :: Monad m => Int -> m a -> m [a]
replicateM n = sequence . replicate n 

replicateM_ :: Monad m => Int -> m a -> m ()
replicateM_ n = sequence_ . replicate n





--7) Writer
--1. Definir la siguiente función que dado un número se queda con los elementos que son mayores
--a éste, informando qué elementos son agregados al resultado:
--mayoresA :: Int -> [Int] -> Writer [String] [Int]

--2. Dada la siguiente definición para obtener el Máximo Común Dividor
--gcd’ :: Int -> Int -> Int
--gcd’ a b = if b == 0 then a else gcd’ b (a ‘mod‘ b)
--transformarla a una versión monádica que indique el valor de los parámetros en cada mo-
--mento













