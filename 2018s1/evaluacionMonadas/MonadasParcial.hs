--                                                   INSTANCIA DE EVALUACIÓN MÓNADAS

--Se desea modelar un lenguaje de programación llamado miniGobstones. En minigobstones un programa es una secuencia de comandos que se ejecutan --sobre una fila finita de celdas, que llamaremos tablero unidimensional. En las celdas encontramos bolitas. Además el tablero posee un cabezal --que apunta siempre a una celda del mismo, puede ejecutaar diversas operaciones sobre la celda actual.
--El módulo miniGobstones (no es un tipo abstracto) posee actualmente implementadas las siguientes funciones:

--poner :: Command
--Pone una bolita en la celda actual

sacar :: Command 
--Saca una bolita de la celda actual. Precond: la celda debe tener bolitas
-- modify necesita una función (a -> a) donde a es el tipo del State, por lo tanto como el State:: [Int], podria ser un map, pero en este caso es zaraza
sacar = modify (id)

-- def solo para que compile
mover :: Dir -> Command
--Mueve el cabezal una celda hacia la dirección indicada. Precond: debe haber una celda contigua en esa dirección
mover Der = State $ \s -> ((),s)
mover Izq = State $ \s -> ((),s)

--nroBolitas :: Exp Int -> Int
--Devuelve la cantidad de bolitas de la celda actual
--nroBolitas f = do
--                 t <- get
--                 f t 

-- def solo para que compile
puedeMover :: Dir -> Exp Bool
puedeMover Izq = (\ t -> True)
puedeMover Der = (\ t -> False)
--Indica si es posible moverse hacia cierta dirección

-- Se agrega para que compile
newtype State s a = State (s -> (a,s))

instance Monad (State s) where
  return x = State $ \s -> (x,s)
  (State h) >>= f = State $ \s -> let (a, newState) = h s
                                      (State g) = f a
                                      in g newState

modify :: (s -> s) -> State s ()
modify f = State (\s -> ((), f s))

get :: State s s
get = State $ \s -> (s,s)

--Además sabemos que:

--Una dirección esta definida como:

data Dir = Izq | Der

--Un commando esta definido como:

-- Board es cualquier cosa
type Board = [Int]


type Command = State Board ()

--Una expresión cuyo resultado depende del estado del tablero esta definida como:

type Exp a = Board -> a

--El tipo Board es abstracto y no nos intereza su interfaz

--Ejercicios:

--Implementar las siguientes funciones que se añadirán al modulo minigobstones:

skyp :: Command
--Devuelve un commando que no modifica el tablero
skyp = return ()



sequence', sequence'', sequence''' :: [Command] -> Command
--Dada una secuencia de commandos devuelve el commando que resulta de ejecutar cada comando que
--conforma la secuencia, en orden, desde el primero hasta el último
sequence' [] = return ()
sequence' (m:mx) = do
                    m
                    sequence' mx

sequence_' :: Monad m => [m a] -> m ()
sequence_' = foldr (>>) (return ())
 
sequence'' = foldr (>>) (return ())

sequence''' = sequence_


repeat', repeat'' :: Int -> Command -> Command
--Dada una expresión n de tipo número y un commando c devuelve el resultado de ejecutar n veces el commando c
repeat' 0 mx = return ()
repeat' n mx = mx >> repeat' (n-1) mx

repeat'' n = sequence_' . replicate n


while :: Exp Bool -> Command -> Command
--Dada una expresión booleana y una secuencia c, ejecuta c mientras la expresión booleana aplicada al tablero
--actual sea verdadera.
while exp mx = do
                 t <- get
                 if (exp t) then mx >> (while exp mx) 
                            else return ()

hayBolitas :: Exp Bool
--Indica si la celda actual posee bolitas
hayBolitas = (\ t -> nroBolitas t > 0)


irAlExtremo :: Dir -> Command
--Va al extremo del tablero indicado por la dirección dada
irAlExtremo d = while (puedeMover d) (mover d)


--vaciar :: Command
--Vacía todas las celdas del tablero
--vaciar = do
--           irAlExtremo Izq
--           t <- get
--           while (puedeMover Der) (sacarTodas >> mover(Der))

--sacarTodas :: Command
--sacarTodas = while (hayBolitas) (sacar)


execute :: Command -> Board -> Board
--Ejecuta un programa en MiniGobstones, donde dado un tablero inicial se ejecuta el programa sobre dicho tablero y se
--devuelve un tablero resultante.
execute mx t = snd (runState mx t)
--Recordar que exíste la función: runState :: State s a -> s -> (s, a)

runState (State f) s = f s

{-
-}
