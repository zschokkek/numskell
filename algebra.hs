module Algebra
    ( Expr(..)
    , prettyPrint
    , substitute
    , simplify
    , expand
    ) where

data Expr
    = Const Double
    | Var String
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Pow Expr Expr
    | Log Expr
    deriving (Show, Eq)

prettyPrint :: Expr -> String
prettyPrint (Const c) = show c
prettyPrint (Var x) = x
prettyPrint (Add u v) = "(" ++ prettyPrint u ++ " + " ++ prettyPrint v ++ ")"
prettyPrint (Sub u v) = "(" ++ prettyPrint u ++ " - " ++ prettyPrint v ++ ")"
prettyPrint (Mul u v) = "(" ++ prettyPrint u ++ " * " ++ prettyPrint v ++ ")"
prettyPrint (Div u v) = "(" ++ prettyPrint u ++ " / " ++ prettyPrint v ++ ")"
prettyPrint (Pow u v) = "(" ++ prettyPrint u ++ " ^ " ++ prettyPrint v ++ ")"
prettyPrint (Log u) = "log(" ++ prettyPrint u ++ ")"

-- Substitute function for expressions
substitute :: Expr -> String -> Expr -> Expr
substitute (Const c) _ _ = Const c
substitute (Var x) var expr
    | x == var = expr
    | otherwise = Var x
substitute (Add u v) var expr = Add (substitute u var expr) (substitute v var expr)
substitute (Sub u v) var expr = Sub (substitute u var expr) (substitute v var expr)
substitute (Mul u v) var expr = Mul (substitute u var expr) (substitute v var expr)
substitute (Div u v) var expr = Div (substitute u var expr) (substitute v var expr)
substitute (Pow u v) var expr = Pow (substitute u var expr) (substitute v var expr)
substitute (Log u) var expr = Log (substitute u var expr)

-- Simplify function for expressions
simplify :: Expr -> Expr
simplify (Add (Const 0) u) = simplify u
simplify (Add u (Const 0)) = simplify u
simplify (Sub u (Const 0)) = simplify u
simplify (Mul (Const 1) u) = simplify u
simplify (Mul u (Const 1)) = simplify u
simplify (Mul (Const 0) _) = Const 0
simplify (Mul _ (Const 0)) = Const 0
simplify (Add (Const a) (Const b)) = Const (a + b)
simplify (Sub (Const a) (Const b)) = Const (a - b)
simplify (Mul (Const a) (Const b)) = Const (a * b)
simplify (Div (Const a) (Const b)) = Const (a / b)
simplify (Pow (Const a) (Const b)) = Const (a ** b)
simplify (Log (Const a)) = Const (log a)
simplify (Add u v) = Add (simplify u) (simplify v)
simplify (Sub u v) = Sub (simplify u) (simplify v)
simplify (Mul u v) = Mul (simplify u) (simplify v)
simplify (Div u v) = Div (simplify u) (simplify v)
simplify (Pow u v) = Pow (simplify u) (simplify v)
simplify (Log u) = Log (simplify u)
simplify e = e


-- Function to expand algebraic expressions
expand :: Expr -> Expr
expand (Add e1 e2) = Add (expand e1) (expand e2)
expand (Sub e1 e2) = Sub (expand e1) (expand e2)
expand (Mul (Add e1 e2) e3) = Add (expand (Mul e1 e3)) (expand (Mul e2 e3))
expand (Mul e1 (Add e2 e3)) = Add (expand (Mul e1 e2)) (expand (Mul e1 e3))
expand (Mul e1 e2) = Mul (expand e1) (expand e2)
expand (Div e1 e2) = Div (expand e1) (expand e2)
expand (Pow e1 e2) = Pow (expand e1) (expand e2)
expand (Log e) = Log (expand e)
expand e = e

