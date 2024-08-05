--TODO: substition? more complexi math problems? export algebra? if export, can we 
--TODO: series? series.hs?  
import Test.HUnit
import Algebra


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

-- Compute the n-th order Taylor series expansion around a point
-- Compute the n-th order Taylor series expansion around a point
taylorSeries :: Expr -> String -> Double -> Int -> Expr
taylorSeries f x a n
    | n < 0     = error "Order of Taylor series must be non-negative"
    | otherwise = foldr Add (Const 0) terms
  where
    terms = [term k | k <- [0..n]]
    term k = Div (Mul (diff k) (Pow (Sub (Var x) (Const a)) (Const (fromIntegral k)))) (factorial k)
    diff 0 = f
    diff k = differentiate (diff (k-1)) x
    factorial 0 = Const 1
    factorial k = Const (fromIntegral (product [1..k]))

-- Define an example expression f(x) = x^2 + 2x + 1
f :: Expr
f = Add (Add (Pow (Var "x") (Const 2)) (Mul (Const 2) (Var "x"))) (Const 1)

-- Differentiate f with respect to x
dfdx :: Expr
dfdx = differentiate f "x"

-- Integrate f with respect to x
intF :: Expr
intF = integrate f "x"

-- Define the main function for testing
main :: IO ()
main = do
    runTestTT tests
    return ()

-- Define tests for Taylor series
tests :: Test
tests = TestList [ testTaylorSeries1 ]

-- Test cases
testTaylorSeries1 :: Test
testTaylorSeries1 = TestCase $ do
    let expr = Var "x"
    let result = taylorSeries expr "x" 0 1
    let expected = Add (Const 0.0) (Var "x")
    assertEqual "Taylor series of f(x) = x at x = 0, n = 1" expected (simplify result)

-- Simplify the result
simplifyResult :: Expr -> Expr
simplifyResult = simplify