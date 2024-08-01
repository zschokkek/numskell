--TODO: substition? more complexi math problems? export algebra? if export, can we 
--TODO: series? series.hs?  


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

-- Function to perform symbolic differentiation
differentiate :: Expr -> String -> Expr
differentiate (Const _) _ = Const 0
differentiate (Var x) v
    | x == v = Const 1
    | otherwise = Const 0
differentiate (Add u v) x = Add (differentiate u x) (differentiate v x)
differentiate (Sub u v) x = Sub (differentiate u x) (differentiate v x)
differentiate (Mul u v) x = Add (Mul (differentiate u x) v) (Mul u (differentiate v x))
differentiate (Div u v) x = Div (Sub (Mul (differentiate u x) v) (Mul u (differentiate v x))) (Pow v (Const 2))
differentiate (Pow u (Const n)) x = Mul (Mul (Const n) (Pow u (Const (n - 1)))) (differentiate u x)
differentiate (Pow u v) x = Mul (Pow u v) (Add (Mul v (differentiate u x)) (Mul (differentiate v x) (Log u)))
differentiate (Log u) x = Div (differentiate u x) u

-- Function to perform symbolic integration (basic implementation)
integrate :: Expr -> String -> Expr
integrate (Const c) x = Mul (Const c) (Var x)
integrate (Var x) x' 
    | x == x' = Div (Pow (Var x) (Const 2)) (Const 2)
    | otherwise = Mul (Var x) (Var x')
integrate (Add u v) x = Add (integrate u x) (integrate v x)
integrate (Sub u v) x = Sub (integrate u x) (integrate v x)
integrate (Mul (Const c) v) x = Mul (Const c) (integrate v x)
integrate (Mul v (Const c)) x = Mul (Const c) (integrate v x)
integrate (Pow (Var x) (Const n)) x' 
    | x == x' = Div (Pow (Var x) (Const (n + 1))) (Const (n + 1))
    | otherwise = error "Unsupported integral form"
integrate e _ = error $ "Cannot integrate expression: " ++ show e

-- Define an example expression f(x) = x^2 + 2x + 1
f :: Expr
f = Add (Add (Pow (Var "x") (Const 2)) (Mul (Const 2) (Var "x"))) (Const 1)

-- Differentiate f with respect to x
dfdx :: Expr
dfdx = differentiate f "x"

-- Integrate f with respect to x
intF :: Expr
intF = integrate f "x"

main :: IO ()
main = do
    putStrLn $ "Expression: " ++ prettyPrint f
    putStrLn $ "Derivative: " ++ prettyPrint dfdx
    putStrLn $ "Integral: " ++ prettyPrint intF