module Term where

data Term = Const Int Int| Var String | Fun String Term | Integ Term Term | Sum Term Term | Mult Term Term | Exp Term Int

t:: Term
t = Var "t"
u:: Term
u = Var "u"
v:: Term
v = Var "v"
w:: Term
w = Var "w"
x:: Term
x = Var "x"
y:: Term
y = Var "y"
z:: Term
z = Var "z"

showTerm:: Term -> String
showTerm (Const n 1) = show n
showTerm (Const x p) = show x ++ "/" ++ show p
showTerm (Var x) = x
showTerm (Fun f t) = f ++ "(" ++ showTerm t ++ ")"
showTerm (Integ t1 x) = "S" ++ showTerm t1 ++ "d" ++ showTerm x 
showTerm (Sum p1 p2) = (showTerm p1) ++ "+" ++ (showTerm p2)
showTerm (Mult p1 p2) = (showTerm p1) ++ " " ++ (showTerm p2)
showTerm (Exp (Fun "sen" t) n) = "sen" ++ "^" ++ (show n) ++ "(" ++ showTerm t ++ ")"
showTerm (Exp (Fun "cos" t) n) = "cos" ++ "^" ++ (show n) ++ "(" ++ showTerm t ++ ")"
showTerm (Exp t1 n) = (showTerm t1) ++ "^" ++ show n
instance Show Term where show p1 = showTerm p1

(!+):: Term -> Term -> Term
(!+) (Const n 1) (Const 0 m) = Const n 1
(!+) (Const 0 m) (Const p 1) = Const p 1
(!+) (Const n 1) (Const p 1) = Const (n+p) 1
(!+) (Const n m) (Const p q) = Const (n*q+p*m) (q*m)
(!+) (Var x) (Const p q) = Sum (Var x) (Const p q)
(!+) (Const p q) (Var x) = Sum (Var x) (Const p q)
(!+) (Var x) (Var y) = Sum (Var x) (Var y)
(!+) (Sum p q) (Sum t u) = (Sum (Sum p q) (Sum t u))
(!+) (Sum p q) (Const x y) = (Sum (Sum p q) (Const x y))
(!+) (Sum p q) (Var x) = (Sum (Sum p q) (Var x))
(!+) (Var p) (Sum x y) = (Sum (Var p) (Sum x y))
(!+) (Const x y) (Sum p q) = (Sum (Const x y) (Sum p q))
(!+) (Mult p q) (Mult t u) = (Sum (Mult p q) (Mult t u))

(!*):: Term -> Term -> Term
(!*) (Const 0 m) (Const p q) = Const 0 1
(!*) (Const n m) (Const 0 q) = Const 0 1
(!*) (Const n m) (Const p q) = Const (n*p) (q*m)
(!*) (Var x) (Var y) = Mult (Var x) (Var y)
(!*) (Var x) (Const p q) = Mult (Const p q) (Var x)
(!*) (Const p q) (Var x) = Mult (Const p q) (Var x)
(!*) (Mult (Const p q) (Var x)) (Const m n) = Mult (Const (p*m) (q*n)) (Var x)
(!*) (Const m n) (Mult (Const p q) (Var x)) = Mult (Const (p*m) (q*n)) (Var x)
(!*) (Sum a b) (Sum f g) = (Mult (Sum a b) (Sum f g))
(!*) (Mult a b) (Mult f g) = (Mult (a!*f) (b!*g))


(!/):: Term -> Term -> Term
(!/) (Const 0 m) (Const p q) = Const 0 1
(!/) (Const n m) (Const 0 q) = error "Division entre 0"
(!/) (Const n m) (Const p q) = Const (n*q) (m*p)

