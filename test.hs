{-|
Module      : Test
Description : Testing framework for statistical computations
Copyright   : (c) Kyle Zschokke, 2025
License     : BSD3

This module provides comprehensive testing for our statistical computing system,
including unit tests, property-based tests, and numerical stability tests.
-}

module Test where

import Test.QuickCheck
import Test.HUnit
import qualified Numeric.LinearAlgebra.Algorithms as LA
import qualified Numeric.Matrix as M
import qualified Numeric.Vector as V
import qualified Statistics as S

-- | Property-based tests for vector operations
prop_vector_addition_commutative :: V.Vector -> V.Vector -> Property
prop_vector_addition_commutative v1 v2 =
    V.length v1 == V.length v2 ==>
        case V.add v1 v2 of
            Nothing -> True  -- Different lengths handled properly
            Just sum1 -> case V.add v2 v1 of
                            Nothing -> False  -- Should not happen
                            Just sum2 -> sum1 == sum2

-- | Property-based tests for matrix operations
prop_matrix_multiplication_associative :: M.Matrix -> M.Matrix -> M.Matrix -> Property
prop_matrix_multiplication_associative a b c =
    compatibleDimensions a b && compatibleDimensions b c ==>
        case (M.multiply a b >>= M.multiply c,
              M.multiply b c >>= M.multiply a) of
            (Just m1, Just m2) -> matrixAlmostEqual m1 m2 1e-10
            _ -> True  -- Incompatible dimensions handled properly

-- | Numerical stability tests
test_numerical_stability :: Test
test_numerical_stability = TestList [
    -- Test condition number computation
    TestCase $ do
        let m = createIllConditionedMatrix 1000
        case LA.condition m of
            Nothing -> assertFailure "Failed to compute condition number"
            Just c -> assertBool "Condition number in expected range" 
                                (c > 1000 && c < 1001),
                                
    -- Test eigenvalue computation stability
    TestCase $ do
        let m = createDefectiveMatrix
        case LA.eigenvalues m of
            Nothing -> assertFailure "Failed to compute eigenvalues"
            Just vals -> assertBool "Eigenvalues accurate within tolerance"
                                  (eigenvaluesAccurate m vals 1e-8)
    ]

-- | Statistical accuracy tests
test_statistical_accuracy :: Test
test_statistical_accuracy = TestList [
    -- Test normal distribution computations
    TestCase $ do
        let xs = generateNormalSample 1000 0 1
        let mean = S.mean xs
        let std = S.standardDeviation xs
        assertBool "Sample mean within 3 sigma" (abs mean < 3/sqrt 1000)
        assertBool "Sample std within 3 sigma" (abs (std - 1) < 3/sqrt 1000),

    -- Test regression accuracy
    TestCase $ do
        let (x, y) = generateLinearData 100 2.5 1.3 0.1
        case S.fitLinearModel x y of
            Nothing -> assertFailure "Failed to fit linear model"
            Just model -> do
                assertBool "Slope estimate accurate" 
                    (abs (S.coefficients model V.! 1 - 2.5) < 0.2)
                assertBool "Intercept estimate accurate"
                    (abs (S.coefficients model V.! 0 - 1.3) < 0.2)
    ]

-- | Helper functions for testing

-- | Check if two matrices are approximately equal
matrixAlmostEqual :: M.Matrix -> M.Matrix -> Double -> Bool
matrixAlmostEqual m1 m2 tol = 
    let diff = case M.subtract m1 m2 of
                   Nothing -> error "Incompatible dimensions"
                   Just d -> maximum [abs x | row <- M.toLists d, x <- row]
    in diff < tol

-- | Create a matrix with known condition number
createIllConditionedMatrix :: Double -> M.Matrix
createIllConditionedMatrix cond = 
    let n = 10  -- Size of matrix
        diag = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1/cond]  -- Diagonal elements
    in M.fromLists [[if i == j then diag !! i else 0 | j <- [0..n-1]] 
                                                     | i <- [0..n-1]]

-- | Generate random sample from normal distribution
generateNormalSample :: Int -> Double -> Double -> V.Vector
generateNormalSample n mu sigma = undefined  -- TODO: Implement Box-Muller transform

-- | Generate linear regression test data
generateLinearData :: Int -> Double -> Double -> Double -> (M.Matrix, V.Vector)
generateLinearData n slope intercept noise = undefined  -- TODO: Implement

-- | Check if computed eigenvalues are accurate
eigenvaluesAccurate :: M.Matrix -> V.Vector -> Double -> Bool
eigenvaluesAccurate m vals tol = undefined  -- TODO: Implement

-- | Run all tests
main :: IO ()
main = do
    -- Run QuickCheck tests
    quickCheck prop_vector_addition_commutative
    quickCheck prop_matrix_multiplication_associative
    
    -- Run HUnit tests
    counts <- runTestTT $ TestList [
        test_numerical_stability,
        test_statistical_accuracy
        ]
    putStrLn $ "Tests completed: " ++ show counts
