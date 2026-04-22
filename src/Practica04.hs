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
conflict = undefined

--Ejercicio 2
success :: Estado -> Bool
success = undefined

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
elim = undefined

--Ejercicio 5
red :: Estado -> Estado
red = undefined

--Funciones auxiliares.
esUnitaria :: Clausula -> Bool
esUnitaria [] = False
esUnitaria [x] = True
esUnitaria xs = False

obtenerNombre :: Literal -> String
obtenerNombre (Var x) = x
obtenerNombre (Not (Var x)) = x

tieneInterpretacion :: String -> Interpretacion -> Bool
tieneInterpretacion _ [] = False
tieneInterpretacion x ((y,b):ys) = if x == y
                                then True
                                else tieneInterpretacion x ys

obtenerLiteral :: Clausula -> Literal
obtenerLiteral [x] = x
obtenerLiteral xs = Var "foo"

darValor :: Clausula -> Interpretacion
darValor [Var p] = [(p, True)]
darValor [Not (Var p)] = [(p, False)]

acumularClausula :: Estado -> Estado -> Estado
acumularClausula (_,xs) (12, ys) = (12, xs ++ ys)

construirArbolDPLL :: Estado -> ArbolDPLL
construirArbolDPLL estado =
                | conflict estadi = Node estado Void
                | sucess estado = Node estado Void
                | segundoElemento propuesto /= segundoElemento estado = Node estado(construirArbolDPLL propuesto)
                otherwise Branch estado (construirArbolDPLL izq) (construirArbolDPLL der)
                where
                    propuesto = red (elim (unit estado))
                    literal = heuristicsLiteral (segundoElemento estado)
                    [izq, der] = sep literal estado 

segundoElemento :: (a,b) -> b
segundoElemento (_,y) = y

explorarArbolDPLL :: ArbolDPLL -> Estado
explorarArbolDPLL (Node estado Void) = estado
explorarArbolDPLL (Node _ subarbol) = explorarArbolDPLL subarbol
explorarArbolDPLL (Branch estado izq der) = if conflicto (explorarArbolDPLL izq)
                                        then explorarArbolDPLL der
                                        else explorarArbolDPLL izq

--Ejercicio 6
sep :: Literal -> Estado -> (Estado, Estado)
sep = undefined

--IMPLEMENTACION PARTE 2


--Ejercicio 1
heuristicsLiteral :: [Clausula] -> Literal
heuristicsLiteral = undefined

--EJERCICIO 2
dpll :: [Clausula] -> Interpretacion
dpll = undefined

--EXTRA
dpll2 :: Prop -> Interpretacion
dpll2 = undefined