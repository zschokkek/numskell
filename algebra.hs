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

-- Test cases
main :: IO ()
main = do
    let expr1 = Mul (Add (Var "x") (Var "y")) (Var "z")
    let expr2 = Mul (Add (Var "a") (Var "b")) (Add (Var "c") (Var "d"))
    prettyPrint expr1