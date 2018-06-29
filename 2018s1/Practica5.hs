import System.Random 
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

compose :: (a -> a) -> (a -> a) -> (a -> a) 
compose g f = appEndo ( (Endo g) `mappend` (Endo f) )

concatMap' :: (a -> [b]) -> [a] -> [b]
concatMap' f = mconcat . map f

applyN :: Int -> (a -> a) -> a -> a
applyN n = appEndo . mconcat . map Endo . replicate n

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


--                                                          LIST - APPLICATIVE FUNCTOR
--3)
--3.1)
-- 1. Dada la siguiente implementación de Applicative Functor para listas

(<$>) :: Functor f => (a->b) -> f a -> f b
(<$>) = fmap


class (Functor f) => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b


instance Applicative [] where
  pure x = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]



--indicar el resultado de las siguientes expresiones

--[(*2),(+100)] <*> [1,2,3] = [2,4,6,101,102,103]
--[(+),(*)] <*> [1,2] <*> [3,4] = [4,5,5,6,3,4,6,8]
--(++) <$> ["hola","chau","gracias"] <*> ["?","!","."] = ["hola?","hola!","hola.","chau?","chau!","chau.","gracias?","gracias!","gracias."]
--filter (>50) $ (*) <$> [2,5,10] <*> [8,10,11] = [55,80,100,110]


--2. Dada la siguiente implementación de Applicative Functor para listas

newtype ZipList a = ZipList [a]

getZipList (ZipList xs) = xs

instance Functor ZipList where
   fmap f (ZipList xs) = ZipList (fmap f xs)

instance Applicative ZipList where
   pure x = ZipList (repeat x)
   ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)

--indicar el resultado de las siguientes expresiones

--getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,100,100] = [101,102,103]    
--getZipList $ (+) <$> ZipList [1,2,3] <*> pure 100 = [101,102,103]
--getZipList $ max <$> ZipList [1,2,3,4,5,3] <*> ZipList [5,3,1,2] = [5,3,3,4]
--getZipList $ (,,) <$> ZipList "hola" <*> ZipList "que" <*> ZipList "tal" = [('h','q','t'),('o','u','a'),('l','e','l')]


--3. Describir el comportamiento de la siguiente definición (PENDIENTE)
sequenceA :: (Applicative f) => [f a] -> f [a]
sequenceA [] = pure []
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs

--4. Escribir la versión monádica de sequenceA, llamada simplemente sequence (PENDIENTE)

--5. (Desafío) Demostrar que la siguiente definición es igual a la anterior (PENDIENTE)

liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' = foldr (liftA2 (:)) (pure [])

-------------------------------------------------------------------------------------------------------------------------------------------

--3)                                                    MAYBE - APPLICATIVE FUNCTOR 

--3.1) 

--Dada la siguiente implementación de Applicative Functor para Maybe indicar el resultado de las siguientes expresiones
instance Applicative Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  (Just f) <*> m = fmap f m

--Just (+3) <*> Just 9 = Just 12
--pure (++"hahah") <*> Nothing = Nothing
--Nothing <*> Just "woot" = Nothing
--pure (.) <*> Just (+1) <*> Just (+2) <*> 3 = ROMPE!!!!!!!!!!!!


--3.2)
--                                                           MAYBE MONAD

--Dadas las siguientes definiciones dar una definición monádica para f:
tailM :: [a] -> Maybe [a]
tailM [] = Nothing
tailM (x:xs) = Just xs

initM :: [a] -> Maybe [a]
initM [] = Nothing
initM [x] = Just []
initM (x:xs) = do
                 zs <- (initM xs)
                 Just (x:zs)


f :: Maybe [Int]
f = do
      xs <- tailM [1,2,3,4,5]
      ys <- initM xs
      zs <- tailM ys
      js <- initM zs
      return js

f' = tailM [1..5] >>= initM >>= tailM >>= initM
-------------------------------------------------------------------------------------------------------------------------------------------

--                                                           4) LIST MONAD

--La definición de Monad de listas es:
--instance Monad [] where
--  return x = [x]
--  xs >>= f = concat (map f xs)

--4.1) List Monad
--a) Indicar el resultado de las siguientes expresiones:
--[3,4,5] >>= \x -> [x,-x] = [3,-3,4,-4,5,-5]  
--[] >>= \x -> ["bad","mad","rad"] = []
--[1,2,3] >>= \x -> [] = []
--[1,2,3] >>= (\x -> [x,x]) >>= (\y -> return [y,y*y]) = [[1,1],[1,1],[2,4],[2,4],[3,9],[3,9]]

--4.2) Definir las siguientes funciones utilizando la mónada de listas

productoCartesiano :: [a] -> [b] -> [(a, b)]
productoCartesiano xs ys = do
                           x <- xs
                           y <- ys
                           return (x,y) 


data Lado = Cara | Seca deriving (Show)

tiradas :: Int -> [[Lado]]
tiradas 0 = return []
tiradas n =  do
               xs <- tiradas (n-1)
               [Seca:xs, Cara:xs]


-- filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
-- específicamente para esta mónada
-- filterM :: (a -> [Bool]) -> [a] -> [[a]]

--(Desafío) Se recomienda usar filterM (ver Hoogle)
--Devuelve todas las sublistas (pensar en subconjuntos) posibles.
powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs

---------------------------------------------------------------------------------------------------------------------------------

--                                                        5) READER MONAD
--5.1) Functor
--Dada la siguiente instancia de Functor
instance Functor ((->) r) where
   fmap f g = (\x -> f (g x))
-- Equivalente a fmap = (.)

--Indicar el resultado de las siguientes expresiones:
--fmap (*3) (+100) 1 = 303
--fmap (show . (*3)) (*100) 1 = "300"
--fmap (replicate 3) (const 3) 3 = [3,3,3]
--fmap (replicate 3) (+3) 3 = [6,6,6] 
--fmap (const 3) (const 3) 3 = 3 

--5.2) Expresiones Aritméticas con variables
--Dada la siguiente representación de expresiones aritméticas

--data Env = Map String Int

--data Exp = Sum Exp Exp | Var String | Const Int deriving (Show)


--Completar!!!!!!
--getValue :: String -> Env -> Int
--getValue s e = case lookup s e of
--               Nothing -> 0
--               Just v  -> v

--completar la definición de eval :: Exp -> Reader Env Int (PENDIENTE)

--5.3)
--Propiedades
--Demuestre que para la instancia Functor de (->) r las siguientes propiedades (PENDIENTE)

--fmap id = id

--fmap (f . g) = fmap f . fmap g


----------------------------------------------------------------------------------------------------------------------------------

--                                                 6) WRITER

--1) Definir la siguiente función que dado un número se queda con los elementos que son mayores a éste, informando qué elementos son agregados al resultado:

newtype Writer w a = Writer (a,w)
-- w es un monoide

runWriter (Writer (a,b)) = (a,b)

instance (Monoid w) => Monad (Writer w) where  
    return x = Writer (x, mempty)  
    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')  


tell :: Monoid w => w -> Writer w ()
tell w = Writer ((), w)

mayoresA :: Int -> [Int] -> Writer [String] [Int]
mayoresA n = foldr g (return [])
  where g x r = if x > n
                   then tell ["Me quedo con: " ++ show x] >> r
                        -- es lo mismo que
                        -- do 
                        -- tell ["Me quedo con: " ++ show x]
                        -- r
                   else r

--2) Dada la siguiente definición para obtener el Máximo Común Dividor

gcd' :: Int -> Int -> Int
gcd' a b = if b == 0 then a else gcd' b (a `mod` b)

--transformarla a una versión monádica que indique el valor de los parámetros en cada momento

gcd'' :: Int -> Int -> Writer [Int] Int
gcd'' a b =
          do
            tell [a, b]
            if b == 0 then return a else gcd'' b (a `mod` b)

----------------------------------------------------------------------------------------------------------------------------------

--7)                                                     STATE MONAD


--7.1)Stack
--Dada la siguiente definición de una Stack

--newtype Stack a = [a]

--newtype Stack a = S [a]

type Stack a = [a]

newtype State s a = State (s -> (a,s))

instance Monad (State s) where
  return x = State $ \s -> (x,s)
  (State h) >>= f = State $ \s -> let (a, newState) = h s
                                      (State g) = f a
                                      in g newState

runState (State f) s = f s


-- 1) Transformar dicha interfaz a una versión monádica con estado

-- pop :: Stack -> (Int,Stack)
-- pop (x:xs) = (x,xs)
-- push :: Int -> Stack -> Stack
-- push a xs = a:xs

-- Esto esta en slice y no funciona
-- pop :: State (Strack a) a
-- pop xs (head xs, tail xs)

pop :: State (Stack a) a
pop = State (\xs -> (head xs, tail xs) )

push :: a -> State (Stack a) ()
push x = State (\ xs -> ((), x:xs) ) 
     
modify :: (s -> s) -> State s ()
modify f = State (\s -> ((), f s))


{-
f'' :: State (Stack Int) Int
f'' = do
        push 4
        pop
        x <- pop
        return x

-}

dropN :: Int -> State (Stack a) ()
dropN 0 = return ()
dropN n = do
            pop
            dropN (n-1)
            

takeN :: Int -> State (Stack a) (Stack a)
takeN 0 = return [] 
takeN n = do
            x  <- pop
            xs <- takeN (n-1)
            return (x:xs)


push' :: a -> State (Stack a) ()
push' x = modify (x:) 

incr :: State Int ()
incr = modify (+1)

get :: State s s
get = State $ \s -> (s,s)

put :: s -> State s ()
put newState = State $ \s -> ((), newState) 

-- vacia el estado pero lo guarda en el stack de datos (el return)
vaciar :: State (Stack a) (Stack a)
vaciar = do
         xs <- get
         put []
         return xs
           
--2) implementar con esa interfaz las siguientes operaciones

dropN' :: Int -> State (Stack a) ()
dropN' 0 = return ()
dropN' n = do
             pop
             dropN' (n-1)

takeN' :: Int -> State (Stack a) (Stack a)
takeN' 0 = return []
takeN' n = do
             x  <- pop
             xs <- takeN' (n-1)
             return (x:xs)



randomSt :: (RandomGen g, Random a) => State g a 
randomSt = State random

threeCoins :: State StdGen (Int,Int,Int)
threeCoins = do
               a <- randomSt
               b <- randomSt
               c <- randomSt
               return (a,b,c)




--7.2                                               GOBSTONES / MONAD STATE
--1) Dar una implementación de las operaciones de Gobstones donde el estado sea un tablero y los comandos tengan tipo State Tablero ().


data Color = Azul | Negro deriving (Show)
data Dir = Izq | Der deriving (Show)
data Celda = C Int Int deriving (Show) 
data Tablero = T [(Int,Celda)] Int deriving (Show)
--                  fila      cabezal


--tableroVacio :: Int -> Int -> Tablero
--tableroVacio n m = T [()] 0

--moverAux :: Dir -> Tablero -> Tablero
-- ......

ponerAux :: Color -> Tablero -> Tablero 
ponerAux c (T xs n)  = T (map g xs) n
  where g (i, celda) = if i == n
                          then (i, poner'' c celda) 
                          else (i, celda)

poner'' Negro (C n a) = C (n+1) a
poner'' Azul  (C n a) = C n (a+1) 


-- Esto está bien
poner :: Color -> State Tablero ()
poner c = modify (ponerAux c)

-- Esto está bien
--mover :: Dir -> State Tablero ()
--mover d = modify moverAux d

--nroBolitas :: Color -> Tablero -> Int


--2) Escribir el siguiente programa con dicha implementación, que se ejecute sobre un tablero inicial vacío.

-- program {
--           PonerN(10, Negro)
--           PonerN(5, Azul)
--           moverN(3, Der)
--           if (puedeMover(Der)) { mover(Der) }
-- }

puedeMover :: Dir -> Tablero -> Bool
puedeMover Izq (T xs n) = n > 0
puedeMover Der (T xs n) = n < (length xs - 1)

program :: State Tablero ()
program = do
            ponerNT 10 Negro
            ponerNT 5 Azul
            moverNT 3 Der
            t <- get' --get de a retorna el state de tipo a
            when (puedeMover Der t) $ mover Der

ponerNT :: Int -> Color -> State Tablero ()
ponerNT 0 _ = return ()
ponerNT n c = do
               poner c
               ponerNT (n-1) c

moverNT :: Int -> Dir -> State Tablero ()
moverNT 0 _ = return ()
moverNT n d = do
                mover d
                moverNT (n-1) d

get' :: State s a
get' = State $ \s -> (s,( ))

----------------------------------------------------------------------------------------------------------------------------------

--                                                   8) FUNCIONES GENÉRICAS

-- Definir las siguientes funciones estándar sobre mónadas en general (ver ejemplos en Hoogle):

void :: Monad m => m a -> m ()
void mx = mx >> return ()

when :: Monad f => Bool -> f () -> f ()
when b mx = if b then mx else return ()


-- Esto es fmap
liftM :: Monad m => (a -> b) -> m a -> m b
liftM f mx = do
               x <- mx
               return (f x)


liftM2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f mx my = do
                   x <- mx
                   y <- my
                   return $ f x y 


appM :: (Monad m) => m (a -> b) -> m a -> m b
appM mf mx = do
               f <- mf
               x <- mx
               return $ f x


join :: (Monad m) => m (m a) -> m a
join mmx = do
             mx <- mmx
             mx

sequence' :: Monad m => [m a] -> m [a]
sequence' = foldr g (return [])
            where g m r = do
                            x <- m
                            xs <- r
                            return (x:xs)


sequence'_ :: Monad m => [m a] -> m ()
sequence'_ = foldr (>>) (return ())


mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f = foldr g (return [])
          where g y r = do
                          x  <- f y
                          xs <- r
                          return (x:xs)


mapM_ :: Monad m => (a -> m b) -> [a] -> m ()
mapM_ f = foldr (\x r -> f x >> r) (return ())


-- En vez de liftM deberia poder usar fmap
filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM f = foldr g (return [])
            where g x r = do
                            b <- (f x)
                            if b then liftM (x:) r
                                 else r


forM :: (Monad m) => [a] -> (a -> m b) -> m [b]
forM = flip mapM 


(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(>=>) f g x = f x >>= g
                    
forever :: Monad m => m a -> m b
forever mx = mx >> forever mx

(>>$) :: Monad m => m a -> m b -> m b                         
(>>$) mx my = do
                mx
                my
                
  
--El código esta bien, pero hay que resolver algo antes
--zipWithM :: Applicative m => (a -> b -> m c) -> [a] -> [b] -> m [c]
--zipWithM f xs ys = sequence (zipWith f xs ys)

-- Revisar los folds // están mal
--foldM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
--foldM f x = foldr (\ y r -> do
--                              z <- r
--                              return (f z y) ) (return x)

--foldM_ :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m ()
--foldM_ f x = foldr (\ y r -> (>>) . (f x) ) (return ())


-- No tipa
--foldM :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
--foldM f z = foldr g (return z)
--            where g rm x = rm >>= (\r -> f r x)

-- Falta void
--foldM_ :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m ()
--foldM_ f z xs = void (foldM f z xs)


replicateM :: Monad m => Int -> m a -> m [a]
replicateM n m = sequence (replicate n m)

replicateM_ :: Monad m => Int -> m a -> m ()
replicateM_ n m = sequence_ (replicate n m)












