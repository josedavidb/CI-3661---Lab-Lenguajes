--Y este el de Algebra.hs
module Algebra
(simplify
,) where

import Term

mcd :: Int -> Int -> Int
mcd a 0 = a
mcd a b = mcd b (rem a b)

simplify:: Term -> Term

simplify (Const p q) = Const (p `div` (mcd p q)) (q `div` (mcd p q))


--Suma
simplify (Sum (Const p q) (Const 0 m)) = simplify (Const p q)
simplify (Sum (Mult p q) (Const 0 m)) = (Mult p q)
simplify (Sum (Exp p q) (Const 0 m)) =  (Exp p q)
simplify (Sum (Sum p q) (Const 0 m)) = simplify (Sum p q)
simplify (Sum (Var p) (Const 0 m)) = (Var p)
simplify (Sum (Fun p q) (Const 0 m)) = (Fun p q)
simplify (Sum (Integ p q) (Const 0 m)) = (Integ p q)

simplify (Sum (Const 0 m) (Const p q)) = simplify (Const p q)
simplify (Sum (Const 0 m) (Mult p q)) = (Mult p q)
simplify (Sum (Const 0 m) (Exp p q)) = (Exp p q)
simplify (Sum (Const 0 m) (Sum p q)) = simplify (Sum p q)
simplify (Sum (Const 0 m) (Var p)) = (Var p)
simplify (Sum (Const 0 m) (Fun p q)) = (Fun p q)
simplify (Sum (Const 0 m) (Integ p q)) = (Integ p q)

simplify (Sum (Const n m) (Const p q)) = simplify (Const (n*q+p*m) (q*m))

--Multiplicacion
simplify (Mult (Const 1 1) (Const p q)) = simplify (Const p q)
simplify (Mult (Const 1 1) (Mult p q)) = (Mult p q)
simplify (Mult (Const 1 1) (Exp p q)) = (Exp p q)
simplify (Mult (Const 1 1) (Sum p q)) = simplify (Sum p q)
simplify (Mult (Const 1 1) (Var p)) = (Var p)
simplify (Mult (Const 1 1) (Fun p q)) = (Fun p q)
simplify (Mult (Const 1 1) (Integ p q)) = (Integ p q)

simplify (Mult (Const p q) (Const 1 1)) = simplify (Const p q)
simplify (Mult (Mult p q) (Const 1 1)) = (Mult p q)
simplify (Mult (Exp p q) (Const 1 1)) = (Exp p q)
simplify (Mult (Sum p q) (Const 1 1)) = simplify (Sum p q)
simplify (Mult (Var p) (Const 1 1)) = (Var p)
simplify (Mult (Fun p q) (Const 1 1)) = (Fun p q)
simplify (Mult (Integ p q) (Const 1 1)) = (Integ p q)

-- simplify (Mult (Const n m) (Const p q)) = Const (n*p) (q*m)
-- simplify (Mult (Var x) (Var y)) = Mult (Var x) (Var y)
-- simplify (Mult (Var x) (Const p q)) = Mult (Const p q) (Var x)
-- simplify (Mult (Const p q) (Var x)) = Mult (Const p q) (Var x)
-- simplify (Mult (Mult (Const p q) (Var x)) (Const m n)) = Mult (Const (p*m) (q*n)) (Var x)
-- simplify (Mult (Const m n) (Mult (Const p q) (Var x))) = Mult (Const (p*m) (q*n)) (Var x)

--Exponenciacion
simplify (Mult (Const p q) (Const 0 m)) = simplify (Const p q)
simplify (Mult (Mult p q) (Const 0 m)) = simplify (Mult p q)
simplify (Mult (Exp p q) (Const 0 m)) = simplify (Exp p q)
simplify (Mult (Fun p q) (Const 0 m)) = simplify (Fun p q)
simplify (Mult (Sum p q) (Const 0 m)) = simplify (Sum p q)
simplify (Mult (Var p) (Const 0 m)) = simplify (Var p)
simplify (Mult (Integ p q) (Const 0 m)) = simplify (Integ p q)

simplify (Exp (Const p q) n) = simplify (Const (p^n) (q^n))


--Distrib

distrib:: Term -> Term

distrib (Sum p q) = (Sum p q)
distrib (Mult (Const x y) (Sum p q) ) = distrib ( ( (Const x y) !* p) !+ ( (Const x y) !* q) )
distrib (Mult (Sum p q) (Const x y)) = distrib ( ( p !* (Const x y)) !+ ( q !* (Const x y)) )
distrib (Mult (Sum p q) (Sum x y) ) = distrib ( p!*x !+ p!*y !+ q!*x !+ q!*y)
-- distrib (Mult  (Sum p q) (Var x)) = distrib ((p !* x)!+(q !* x))
-- distrib (Mult (Var x) (Sum p q)) = distrib ((x !* p)!+(x !* q))

