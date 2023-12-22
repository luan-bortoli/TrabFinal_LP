module Interpreter where 

import Lexer 
import Parser

isValue :: Expr -> Bool 
isValue BTrue = True 
isValue BFalse = True
isValue (Num _) = True 
isValue (Lam _ _ _) = True
isValue _ = False 

subst :: String -> Expr -> Expr -> Expr 
subst x n (Var v) = if (x == v) then
                      n 
                    else 
                      (Var v)
subst x n (Lam v t b) = Lam v t (subst x n b)
subst x n (App e1 e2) = App (subst x n e1) (subst x n e2)
subst x n (Add e1 e2) = Add (subst x n e1) (subst x n e2)
subst x n (Sub e1 e2) = Sub (subst x n e1) (subst x n e2) --Subtração
subst x n (Mult e1 e2) = Mult (subst x n e1) (subst x n e2) -- Multiplicação
subst x n (And e1 e2) = And (subst x n e1) (subst x n e2) -- And &&
subst x n (Or e1 e2) = Or (subst x n e1) (subst x n e2)  -- Or ||
subst x n (Not e1) = Not (subst x n e) -- Not
subst x n (Eq e1 e2) = Eq (subst x n e1) (subst x n e2) -- Iqual
subst x n (Dif e1 e2) = Dif (subst x n e1) (subst x n e2) -- Diferente
subst x n (Men e1 e2) = Men (subst x n e1) (subst x n e2) -- Menor
subst x n (Mai e1 e2) = Mai (subst x n e1) (subst x n e2) -- Maior
subst x n (MenEq e1 e2) = MenEq (subst x n e1) (subst x n e2) -- Menor Igual
subst x n (MaiEq e1 e2) = MaiEq (subst x n e1) (subst x n e2) -- Maior Igual
subst x n (If e1 e2 e3) = If (subst x n e1) (subst x n e2) (subst x n e3)
subst x n (Paren e) = Paren (subst x n e)
subst x n (Let v e1 e2) = Let v (subst x n e1) (subst x n e2)
subst x n (Ternario v e1 e2) = Ternario v (subst x n e1) (subst x n e2)
subst x n e = e 

step :: Expr -> Expr 
step (Add (Num n1) (Num n2)) = Num (n1 + n2)
step (Add (Num n) e) = Add (Num n) (step e)
step (Add e1 e2) = Add (step e1) e2 

step (Mult (Num n1) (Num n2)) = Num (n1 * n2)
step (Mult (Num n) e) = Mult (Num n) (step e)
step (Mult e1 e2) = Mult (step e1) e2

step (Sub (Num n1) (Num n2)) = Num (n1 * n2)
step (Sub (Num n) e) = Sub (Num n) (step e)
step (Sub e1 e2) = Sub (step e1) e2

step (And BFalse _) = BFalse 
step (And BTrue e) = e 
step (And e1 e2) = And (step e1) e2

step (Or BTrue _) = BTrue
step (Or _ BTrue) = BTrue
step (Or BFalse BFalse) = BFalse
step (Or BFalse e) = Or BFalse (step e)
step (Or e BFalse) = Or (step e) BFalse
step (Or e1 e2) = Or (step e1) e2

step (Not BTrue) = BFalse
step (Not BFalse) = BTrue
step (Not e) = Not (step e)

step (Eq (Num e1) (Num e2)) | (e1 == e2) = BTrue
                            | otherwise = BFalse
step (Eq (Num e1) e2) = Eq (Num e1) (step e2)
step (Eq e1 (Num e2)) = Eq (step e1) (Num e2)
step (Eq e1 e2) | isBool e1 && isBool e2 = if (e1 == e2) then BTrue else BFalse
                | isBool e1 = Eq e1 (step e2)
                | otherwise = Eq (step e1) e2

step (Dif (Num e1) (Num e2)) | (e1 /= e2) = BTrue
                             | otherwise = BFalse
step (Dif (Num e1) e2) = Dif (Num e1) (step e2)
step (Dif e1 (Num e2)) = Dif (step e1) (Num e2)
step (Dif e1 e2) | isBool e1 && isBool e2 = if (e1 /= e2) then BTrue else BFalse
                 | isBool e1 = Dif e1 (step e2)
                 | otherwise = Dif (step e1) e2

step (Men (Num e1) (Num e2)) | (e1 < e2) = BTrue
                             | otherwise = BFalse
step (Men (Num e1) e2) = Men (Num e1) (step e2)
step (Men e1 (Num e2)) = Men (step e1) (Num e2)
step (Men e1 e2) = Men (step e1) e2

step (Mai (Num e1) (Num e2)) | (e1 > e2) = BTrue
                             | otherwise = BFalse
step (Mai (Num e1) e2) = Mai (Num e1) (step e2)
step (Mai e1 (Num e2)) = Mai (step e1) (Num e2)
step (Mai e1 e2) = Mai (step e1) e2

step (MenEq (Num e1) (Num e2)) | (e1 <= e2) = BTrue
                               | otherwise = BFalse
step (MenEq (Num e1) e2) = MenEq (Num e1) (step e2)
step (MenEq e1 (Num e2)) = MenEq (step e1) (Num e2)
step (MenEq e1 e2) = MenEq (step e1) e2

step (MaiEq (Num e1) (Num e2)) | (e1 >= e2) = BTrue
                               | otherwise = BFalse
step (MaiEq (Num e1) e2) = MaiEq (Num e1) (step e2)
step (MaiEq e1 (Num e2)) = MaiEq (step e1) (Num e2)
step (MaiEq e1 e2) = MaiEq (step e1) e2

step (If BFalse e1 e2) = e2 
step (If BTrue e1 e2) = e1 
step (If e e1 e2) = If (step e) e1 e2 
step (Paren e) = e
step (App (Lam x t b) e2) | isValue e2 = subst x e2 b 
                          | otherwise = (App (Lam x t b) (step e2))
step (App e1 e2) = App (step e1) e2
step (Let v e1 e2) | isValue e1 = subst v e1 e2 
                   | otherwise = Let v (step e1) e2
                   
step (Ternario v e1 e2) = case eval v of
                            BTrue -> e1
                            BFalse -> e2

step e = error (show e)

eval :: Expr -> Expr 
eval e | isValue e = e 
       | otherwise = eval (step e)