import Test.HUnit
import Algebra

-- Define an example expression f(x) = x^2 + 2x + 1
f :: Expr
f = Add (Add (Pow (Var "x") (Const 2)) (Mul (Const 2) (Var "x"))) (Const 1)

-- Test cases for simplify function
testSimplify1 :: Test
testSimplify1 = TestCase $ do
    let expr = Add (Const 0) (Var "x")
    let result = simplify expr
    let expected = Var "x"
    assertEqual "Simplify 0 + x" expected result

testSimplify2 :: Test
testSimplify2 = TestCase $ do
    let expr = Mul (Const 1) (Var "x")
    let result = simplify expr
    let expected = Var "x"
    assertEqual "Simplify 1 * x" expected result

testSimplify3 :: Test
testSimplify3 = TestCase $ do
    let expr = Mul (Const 0) (Var "x")
    let result = simplify expr
    let expected = Const 0
    assertEqual "Simplify 0 * x" expected result

testSimplify4 :: Test
testSimplify4 = TestCase $ do
    let expr = Add (Const 2) (Const 3)
    let result = simplify expr
    let expected = Const 5
    assertEqual "Simplify 2 + 3" expected result

testSimplify5 :: Test
testSimplify5 = TestCase $ do
    let expr = Pow (Const 2) (Const 3)
    let result = simplify expr
    let expected = Const 8
    assertEqual "Simplify 2 ^ 3" expected result

-- Add the tests to the tests list
simplifyTests :: Test
simplifyTests = TestList [testSimplify1, testSimplify2, testSimplify3, testSimplify4, testSimplify5]

-- Test cases for substitute function
testSubstitute1 :: Test
testSubstitute1 = TestCase $ do
    let expr = Var "x"
    let result = substitute expr "x" (Const 5)
    let expected = Const 5
    assertEqual "Substitute x with 5 in x" expected result

testSubstitute2 :: Test
testSubstitute2 = TestCase $ do
    let expr = Add (Var "x") (Const 2)
    let result = substitute expr "x" (Const 3)
    let expected = Add (Const 3) (Const 2)
    assertEqual "Substitute x with 3 in x + 2" expected result

testSubstitute3 :: Test
testSubstitute3 = TestCase $ do
    let expr = Mul (Var "y") (Var "x")
    let result = substitute expr "x" (Const 4)
    let expected = Mul (Var "y") (Const 4)
    assertEqual "Substitute x with 4 in y * x" expected result

testSubstitute4 :: Test
testSubstitute4 = TestCase $ do
    let expr = Div (Var "x") (Var "z")
    let result = substitute expr "x" (Const 2)
    let expected = Div (Const 2) (Var "z")
    assertEqual "Substitute x with 2 in x / z" expected result

testSubstitute5 :: Test
testSubstitute5 = TestCase $ do
    let expr = Pow (Var "x") (Const 2)
    let result = substitute expr "x" (Add (Var "y") (Const 3))
    let expected = Pow (Add (Var "y") (Const 3)) (Const 2)
    assertEqual "Substitute x with y + 3 in x ^ 2" expected result

-- Add the tests to the tests list
substituteTests :: Test
substituteTests = TestList [testSubstitute1, testSubstitute2, testSubstitute3, testSubstitute4, testSubstitute5]

-- Define the main function for testing
main :: IO ()
main = do
    runTestTT allTests
    return ()

allTests :: Test
allTests = TestList [simplifyTests, substituteTests]  -- include existing tests