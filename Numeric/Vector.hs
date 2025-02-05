{-|
Module      : Numeric.Vector
Description : Mathematical vector operations
Copyright   : (c) Kyle Zschokke, 2025
License     : BSD3

This module implements mathematical vector operations, building on the low-level
array implementation. It provides a high-level interface for vector algebra
and analysis, hiding the complexity of memory management.
-}

module Numeric.Vector 
    ( -- * Type and Construction
      Vector(..)
    , fromList
    , toList
    , zeros
    , ones
    , linspace
    , generate
      -- * Basic Operations
    , length
    , map
    , zipWith
    , slice
      -- * Vector Algebra
    , add
    , subtract
    , scale
    , dot
    , cross
    , norm
    , normalize
      -- * Vector Analysis
    , sum
    , mean
    , variance
    , standardDev
    ) where

import Prelude hiding (length, map, sum, zipWith)
import qualified Prelude as P
import qualified Array as A
import Control.Monad (join)
import System.IO.Unsafe (unsafePerformIO)

-- | A mathematical vector, built on our UnboxedArray
newtype Vector = Vector (A.UnboxedArray Double)

-- | Show instance formats vectors in a mathematical notation
instance Show Vector where
    show v = "(" ++ (concat . intersperse ", " . map show . toList) v ++ ")"

-- | Construction Functions

-- | Create a vector from a list of values
fromList :: [Double] -> Vector
fromList xs = Vector $ unsafePerformIO $ A.fromList (0, P.length xs - 1) xs

-- | Convert a vector to a list
toList :: Vector -> [Double]
toList (Vector arr) = A.toList arr

-- | Create a vector of zeros
zeros :: Int -> Vector
zeros n = Vector $ unsafePerformIO $ A.zeros n

-- | Create a vector of ones
ones :: Int -> Vector
ones n = Vector $ unsafePerformIO $ A.ones n

-- | Create a vector with evenly spaced values
linspace :: Double -> Double -> Int -> Vector
linspace start end n =
    let step = (end - start) / fromIntegral (n - 1)
    in generate n (\i -> start + fromIntegral i * step)

-- | Generate a vector using a function
generate :: Int -> (Int -> Double) -> Vector
generate n f = fromList [f i | i <- [0..n-1]]

-- | Basic Operations

-- | Get the length of a vector
length :: Vector -> Int
length (Vector arr) = A.size arr

-- | Map a function over a vector
map :: (Double -> Double) -> Vector -> Vector
map f (Vector arr) = Vector $ unsafePerformIO $ do
    let n = A.size arr
    result <- A.createArray (0, n-1)
    mapM_ (\i -> do
        val <- A.readArray arr i
        A.writeArray result i (f val)) [0..n-1]
    return result

-- | Combine two vectors with a function
zipWith :: (Double -> Double -> Double) -> Vector -> Vector -> Maybe Vector
zipWith f v1 v2 
    | length v1 /= length v2 = Nothing
    | otherwise = Just $ Vector $ unsafePerformIO $ do
        let n = length v1
        result <- A.createArray (0, n-1)
        mapM_ (\i -> do
            x <- A.readArray (unwrap v1) i
            y <- A.readArray (unwrap v2) i
            A.writeArray result i (f x y)) [0..n-1]
        return result
  where
    unwrap (Vector arr) = arr

-- | Get a slice of a vector
slice :: Vector -> Int -> Int -> Maybe Vector
slice v start end
    | start < 0 || end >= length v || start > end = Nothing
    | otherwise = Just $ Vector $ unsafePerformIO $ do
        let src = unwrap v
        result <- A.createArray (0, end-start)
        mapM_ (\i -> do
            val <- A.readArray src (i+start)
            A.writeArray result i val) [0..end-start]
        return result
  where
    unwrap (Vector arr) = arr

-- | Vector Algebra

-- | Add two vectors
add :: Vector -> Vector -> Maybe Vector
add = zipWith (+)

-- | Subtract two vectors
subtract :: Vector -> Vector -> Maybe Vector
subtract = zipWith (-)

-- | Scale a vector by a scalar
scale :: Double -> Vector -> Vector
scale k = map (*k)

-- | Compute the dot product of two vectors
dot :: Vector -> Vector -> Maybe Double
dot v1 v2 = do
    multiplied <- zipWith (*) v1 v2
    return $ sum multiplied

-- | Compute the cross product (only for 3D vectors)
cross :: Vector -> Vector -> Maybe Vector
cross v1 v2
    | length v1 /= 3 || length v2 /= 3 = Nothing
    | otherwise = 
        let [x1,y1,z1] = toList v1
            [x2,y2,z2] = toList v2
        in Just $ fromList [y1*z2 - z1*y2, z1*x2 - x1*z2, x1*y2 - y1*x2]

-- | Compute the norm (magnitude) of a vector
norm :: Vector -> Double
norm v = sqrt $ sum $ map (^2) v

-- | Normalize a vector (make it unit length)
normalize :: Vector -> Vector
normalize v = scale (1 / norm v) v

-- | Vector Analysis

-- | Sum of vector elements
sum :: Vector -> Double
sum = P.sum . toList

-- | Mean of vector elements
mean :: Vector -> Double
mean v = sum v / fromIntegral (length v)

-- | Variance of vector elements
variance :: Vector -> Double
variance v =
    let m = mean v
        n = fromIntegral $ length v
    in sum (map (\x -> (x - m)^2) v) / (n - 1)

-- | Standard deviation of vector elements
standardDev :: Vector -> Double
standardDev = sqrt . variance

-- | Helper functions

-- | Apply a binary operation to vectors of the same length
safeBinaryOp :: (Double -> Double -> Double) -> Vector -> Vector -> Maybe Vector
safeBinaryOp f v1 v2
    | length v1 /= length v2 = Nothing
    | otherwise = Just $ Vector $ unsafePerformIO $ do
        let n = length v1
        result <- A.createArray (0, n-1)
        mapM_ (\i -> do
            x <- A.readArray (unwrap v1) i
            y <- A.readArray (unwrap v2) i
            A.writeArray result i (f x y)) [0..n-1]
        return result
  where
    unwrap (Vector arr) = arr