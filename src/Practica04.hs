module Practica04 where

--Sintaxis de la logica proposicional
data Prop = Var String | Cons Bool | Not Prop
            | And Prop Prop | Or Prop Prop
            | Impl Prop Prop | Syss Prop Prop
            deriving (Eq)

instance Show Prop where 
                    show (Cons True) = "⊤"
                    show (Cons False) = "⊥"
                    show (Var p) = p
                    show (Not p) = "¬" ++ show p
                    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
                    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
                    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
                    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

type Literal = Prop
type Clausula = [Literal]

p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

--Definicion de los tipos para la practica
type Interpretacion = [( String , Bool ) ]
type Estado = ( Interpretacion , [Clausula])
data ArbolDPLL = Node Estado ArbolDPLL | Branch Estado ArbolDPLL ArbolDPLL | Void deriving Show

--IMPLEMENTACION PARTE 1
--Ejercicio 1
conflict :: Estado -> Bool
conflict (modelo, []) = False
conflict (modelo, c:xs) = if esClausulaVacia c then True else conflict (modelo, xs)

--Ejercicio 2
success :: Estado -> Bool
success (modelo, []) = True
success (modelo, xs) = False

--Ejercicio 3
unit :: Estado -> Estado
unit (modelo,[]) = (modelo, [])
unit (modelo, c:xs) = if esUnitaria c
                    then if tieneInterpretacion (obtenerNombre (obtenerLiteral c)) modelo
                        then acumularClausula ([], [c]) (unit (modelo, xs))
                        else (modelo ++ darValor c, xs)
                    else acumularClausula ([], [c]) (unit (modelo, xs))

--Ejercicio 4
elim :: Estado -> Estado
elim (mod, claus) = (mod, auxaux mod claus)

--Elimina todas las cláusulas donde aparece un literal
aux :: Literal -> [Clausula] -> [Clausula]
aux _ [] = []
aux lit (x:xs)
  | lit `elem` x = aux lit xs
  | otherwise  = x : aux lit xs

--Aplica el auxiliar al modelo 
auxaux :: Interpretacion -> [Clausula] -> [Clausula]
auxaux [] claus = claus
auxaux (x:xs) claus =
  auxaux xs (aux (toLiteral x) claus)

--Ejercicio 5
red :: Estado -> Estado
red (mod, claus) = (mod, nuevoMod mod claus)

--Transforma el valor de una var a la literal
toLiteral :: (String, Bool) -> Literal
toLiteral (x, True) = Var x
toLiteral (x, False) = Not (Var x)

--Niega una literal
negar :: Literal -> Literal
negar (Var x) = Not (Var x)
negar (Not (Var x)) = Var x 

--Elimina una literal de una clausula
quitarLiteral :: Literal -> Clausula -> Clausula
quitarLiteral _ [] = []
quitarLiteral lit (x:xs)
  | x == lit    = quitarLiteral lit xs
  | otherwise = x : quitarLiteral lit xs

--Aplica quitarliteral a todas las demas clausulas
nuevaClaus :: Literal -> [Clausula] -> [Clausula]
nuevaClaus _ [] = []
nuevaClaus lit (x:xs) =
  quitarLiteral (negar lit) x : nuevaClaus lit xs

--Aplica reduccion al modelo
nuevoMod :: Interpretacion -> [Clausula] -> [Clausula]
nuevoMod [] claus = claus
nuevoMod (x:xs) claus = 
    nuevoMod xs (nuevaClaus (toLiteral x) claus)

--Funciones auxiliares.

--Checa si hay una clausula vacia xd
esClausulaVacia :: Clausula -> Bool
esClausulaVacia [] = True
esClausulaVacia xs = False

--Checa si una clausula tiene una sola literal
esUnitaria :: Clausula -> Bool
esUnitaria [] = False
esUnitaria [x] = True
esUnitaria xs = False

--Obtiene la variable
obtenerNombre :: Literal -> String
obtenerNombre (Var x) = x
obtenerNombre (Not (Var x)) = x

--Revisa si la variable ya tiene valor
tieneInterpretacion :: String -> Interpretacion -> Bool
tieneInterpretacion _ [] = False
tieneInterpretacion x ((y,b):ys) = if x == y
                                then True
                                else tieneInterpretacion x ys

--Saca la literal de una clausula unitaria
obtenerLiteral :: Clausula -> Literal
obtenerLiteral [x] = x
obtenerLiteral xs = Var "foo"

--Da valor a la literal de la clausula unitaria
darValor :: Clausula -> Interpretacion
darValor [Var p] = [(p, True)]
darValor [Not (Var p)] = [(p, False)]

--Combina listas de clausulas
acumularClausula :: Estado -> Estado -> Estado
acumularClausula (_,xs) (l2, ys) = (l2, xs ++ ys)

--Lo mismo que la de arriba pero con modelos :p
acumularModelo :: Estado -> Estado -> Estado
acumularModelo (_,xs) (l2, ys) = (l2, xs ++ ys)

construirArbolDPLL :: Estado -> ArbolDPLL
construirArbolDPLL estado 
                | conflict estado = Node estado Void
                | success estado = Node estado Void
                | segundoElemento propuesto /= segundoElemento estado = Node estado (construirArbolDPLL propuesto)
                | otherwise = Branch estado (construirArbolDPLL izq) (construirArbolDPLL der)
                where
                    propuesto = red (elim (unit estado))
                    literal = heuristicsLiteral (segundoElemento estado)
                    (izq, der) = sep literal estado 

--Devuelve las cláusulas del estado
segundoElemento :: (a,b) -> b
segundoElemento (_,y) = y

explorarArbolDPLL :: ArbolDPLL -> Estado
explorarArbolDPLL (Node estado Void) = estado
explorarArbolDPLL (Node _ subarbol) = explorarArbolDPLL subarbol
explorarArbolDPLL (Branch estado izq der) = if conflict (explorarArbolDPLL izq)
                                        then explorarArbolDPLL der
                                        else explorarArbolDPLL izq

--Ejercicio 6
sep :: Literal -> Estado -> (Estado, Estado)
sep lit (modelo, claus) =
  ( (modelo ++ [(ramear lit, True)], claus)
  , (modelo ++ [(ramear lit, False)], claus)
  )

--Obtiene el nombre de la literal para poder hacer branch
ramear :: Literal -> String
ramear (Var x) = x
ramear (Not (Var x)) = x
    
--IMPLEMENTACION PARTE 2


--Ejercicio 1
heuristicsLiteral :: [Clausula] -> Literal
heuristicsLiteral claus = decidir (separar claus)

--Usa el literal de mayor aparicion
decidir :: [Literal] -> Literal
decidir [] = Var "p"
decidir (x:xs) = mayor xs x (cuentas x (x:xs)) (x:xs)

--Mete todas las clausulas de literales en una misma lista
separar :: [Clausula] -> [Literal]
separar [] = []
separar (x:xs) = x ++ separar xs

--Cuenta cuantas veces aparece un literal
cuentas :: Literal -> [Literal] -> Int
cuentas _ [] = 0
cuentas lit (x:xs)
  | lit == x  = 1 + cuentas lit xs
  | otherwise = cuentas lit xs

--Regresa el literal de mayor aparicion
mayor :: [Literal] -> Literal -> Int -> [Literal] -> Literal
mayor [] lit _ _ = lit
mayor (x:xs) lit max lits
  | cuentas x lits > max = mayor xs x (cuentas x lits) lits
  | otherwise = mayor xs lit max lits


--EJERCICIO 2
dpll :: [Clausula] -> Interpretacion
dpll claus =
  resolver (explorarArbolDPLL (construirArbolDPLL ([], claus)))

--Filtro para saber que se regresa
resolver :: Estado -> Interpretacion
resolver (mod, claus) =
  if conflict (mod, claus)
     then []
     else mod

--EXTRA
dpll2 :: Prop -> Interpretacion
dpll2 pe = dpll (clausulas (fnc pe))


--Apartado de funciones de la practica 3 :v

fnn :: Prop -> Prop
fnn (Var p) = Var p
fnn (Cons a) = Cons a
fnn (Not(Not p)) = fnn p
fnn (Not (Var p)) = Not (Var p)
fnn (Not(Cons a)) = Not (Cons a)
fnn (Or p q) = (Or (fnn p) (fnn q))
fnn (And p q) = (And (fnn p) (fnn q))
fnn (Syss p q) = fnn (And(Impl p q) (Impl q p)) 
fnn (Impl p q) = fnn (Or(Not p) q)
fnn (Not (And p q)) = (Or (fnn(Not p)) (fnn(Not q)))
fnn (Not (Impl p q)) = (And (fnn p) (fnn(Not q)))
fnn (Not (Syss p q)) = fnn (Or (And p (Not q)) (And (Not p) q))
fnn (Not (Or p q)) = (And (fnn(Not p)) (fnn(Not q)))

fnc :: Prop -> Prop
fnc p = distribuir (fnn p)

distribuir :: Prop -> Prop
distribuir (And p q) = And (distribuir p) (distribuir q)
distribuir (Or p q) = distro (distribuir p) (distribuir q)
distribuir p = p

--Funcion auxiliar: Ayud a a distribuir mejor los casos de Or y And
distro :: Prop -> Prop -> Prop
distro (And p q) r = And (distro p r) (distro q r)
distro p (And q r) = And (distro p q) (distro p r)
distro p q = Or p q

clausulas :: Prop -> [[Prop]]
clausulas (And p q) = clausulas p ++ clausulas q
clausulas p = [dups (lister p)]

--Funcion auxiliar: Quita los ORS y pone cada cosa en la lista por separada
lister :: Prop -> [Prop]
lister (Or p q) = lister p ++ lister q
lister p = [p]

-- Funcion auxiliar: Checa si existen dos literales iguales en la lista
existencia :: Eq a => a -> [a] -> Bool
existencia _ [] = False
existencia x (y:ys) 
    | x == y    = True
    | otherwise = existencia x ys

-- Funcion auxiliar: Elimina los duplicados
dups :: Eq a => [a] -> [a]
dups [] = []
dups (x:xs)
    | existencia x xs = dups xs
    | otherwise   = x : dups xs
