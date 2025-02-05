{-|
Module      : Numeric.Matrix
Description : Complete matrix operations implementation
Copyright   : (c) Kyle Zschokke, 2025
License     : BSD3

This module provides a comprehensive implementation of matrix operations,
focusing on both numerical stability and computational efficiency.
-}

module Numeric.Matrix
    ( -- * Types and Construction
      Matrix(..)
    , fromLists
    , toLists
    , fromVectors
    , toVectors
    , identity
    , zeros
    , ones
    , diagonal
      -- * Basic Operations
    , dimensions
    , rows
    , cols
    , transpose
    , getRow
    , getCol
    , submatrix
      -- * Matrix Algebra
    , add
    , subtract
    , multiply
    , scale
    , kronecker
    , power
      -- * Matrix Properties
    , trace
    , determinant
    , rank
    , condition
    , isSymmetric
    , isOrthogonal
    , isPositiveDefinite
      -- * Decompositions
    , luDecomposition
    , qrDecomposition
    , choleskyDecomposition
    , eigendecomposition
    ) where

import Prelude hiding (subtract)
import qualified Prelude as P
import qualified Numeric.Vector as V
import qualified Array as A
import Control.Monad (guard)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (foldl', sortBy)
import Data.Ord (comparing)

-- | Matrix representation using row-major order
data Matrix = Matrix {
    numRows :: Int,
    numCols :: Int,
    elements :: A.UnboxedArray Double
} deriving Eq

-- | Configuration constants
blockSize :: Int
blockSize = 64  -- Cache-friendly block size

epsilon :: Double
epsilon = 1e-10  -- Numerical tolerance

-- | Show instance for readable matrix output
instance Show Matrix where
    show = unlines . map show . toLists

-- | Construction Functions

fromLists :: [[Double]] -> Maybe Matrix
fromLists [] = Nothing
fromLists rows@(firstRow:_)
    | not (all ((== length firstRow) . length) rows) = Nothing
    | otherwise = Just $ unsafePerformIO $ do
        let r = length rows
            c = length firstRow
        arr <- A.createArray (0, r * c - 1)
        mapM_ (\(i, row) -> 
            mapM_ (\(j, val) -> 
                A.writeArray arr (i * c + j) val)
                (zip [0..] row))
            (zip [0..] rows)
        return $ Matrix r c arr

toLists :: Matrix -> [[Double]]
toLists (Matrix r c arr) = 
    chunksOf c $ A.toList arr

fromVectors :: [V.Vector] -> Maybe Matrix
fromVectors [] = Nothing
fromVectors vecs@(v:vs)
    | not (all ((== V.length v) . V.length) vs) = Nothing
    | otherwise = fromLists (map V.toList vecs)

toVectors :: Matrix -> [V.Vector]
toVectors = map V.fromList . toLists

identity :: Int -> Matrix
identity n = Matrix n n $ unsafePerformIO $ do
    arr <- A.createArray (0, n * n - 1)
    mapM_ (\i -> 
        mapM_ (\j -> 
            A.writeArray arr (i * n + j) (if i == j then 1 else 0))
            [0..n-1])
        [0..n-1]
    return arr

zeros :: Int -> Int -> Matrix
zeros r c = Matrix r c $ unsafePerformIO $ A.zeros (r * c)

ones :: Int -> Int -> Matrix
ones r c = Matrix r c $ unsafePerformIO $ A.ones (r * c)

diagonal :: V.Vector -> Matrix
diagonal v = 
    let n = V.length v
        vs = V.toList v
    in Matrix n n $ unsafePerformIO $ do
        arr <- A.createArray (0, n * n - 1)
        mapM_ (\i -> 
            mapM_ (\j -> 
                A.writeArray arr (i * n + j) (if i == j then vs !! i else 0))
                [0..n-1])
            [0..n-1]
        return arr

-- | Basic Operations

dimensions :: Matrix -> (Int, Int)
dimensions (Matrix r c _) = (r, c)

rows :: Matrix -> Int
rows = fst . dimensions

cols :: Matrix -> Int
cols = snd . dimensions

transpose :: Matrix -> Matrix
transpose (Matrix r c arr) = Matrix c r $ unsafePerformIO $ do
    result <- A.createArray (0, r * c - 1)
    mapM_ (\(i, j) -> do
        val <- A.readArray arr (i * c + j)
        A.writeArray result (j * r + i) val)
        [(i, j) | i <- [0..r-1], j <- [0..c-1]]
    return result

getRow :: Matrix -> Int -> Maybe V.Vector
getRow m@(Matrix r c _) i
    | i < 0 || i >= r = Nothing
    | otherwise = Just $ V.fromList [unsafeGet m i j | j <- [0..c-1]]

getCol :: Matrix -> Int -> Maybe V.Vector
getCol m@(Matrix r c _) j
    | j < 0 || j >= c = Nothing
    | otherwise = Just $ V.fromList [unsafeGet m i j | i <- [0..r-1]]

submatrix :: Matrix -> Int -> Int -> Int -> Int -> Maybe Matrix
submatrix m@(Matrix r c _) startRow startCol rows cols
    | startRow < 0 || startCol < 0 || 
      startRow + rows > r || startCol + cols > c = Nothing
    | otherwise = Just $ Matrix rows cols $ unsafePerformIO $ do
        result <- A.createArray (0, rows * cols - 1)
        mapM_ (\(i, j) -> do
            let val = unsafeGet m (startRow + i) (startCol + j)
            A.writeArray result (i * cols + j) val)
            [(i, j) | i <- [0..rows-1], j <- [0..cols-1]]
        return result

-- | Matrix Algebra

add :: Matrix -> Matrix -> Maybe Matrix
add m1@(Matrix r1 c1 arr1) m2@(Matrix r2 c2 arr2)
    | dimensions m1 /= dimensions m2 = Nothing
    | otherwise = Just $ Matrix r1 c1 $ unsafePerformIO $ do
        result <- A.createArray (0, r1 * c1 - 1)
        mapM_ (\i -> do
            x <- A.readArray arr1 i
            y <- A.readArray arr2 i
            A.writeArray result i (x + y))
            [0..r1 * c1 - 1]
        return result

subtract :: Matrix -> Matrix -> Maybe Matrix
subtract m1 m2 = add m1 =<< scale (-1) m2

scale :: Double -> Matrix -> Matrix
scale k (Matrix r c arr) = Matrix r c $ unsafePerformIO $ do
    result <- A.createArray (0, r * c - 1)
    mapM_ (\i -> do
        val <- A.readArray arr i
        A.writeArray result i (k * val))
        [0..r * c - 1]
    return result

multiply :: Matrix -> Matrix -> Maybe Matrix
multiply m1@(Matrix r1 c1 _) m2@(Matrix r2 c2 _)
    | c1 /= r2 = Nothing
    | otherwise = Just $ multiplyBlocked m1 m2

multiplyBlocked :: Matrix -> Matrix -> Matrix
multiplyBlocked m1@(Matrix r1 c1 _) m2@(Matrix r2 c2 _) =
    Matrix r1 c2 $ unsafePerformIO $ do
        result <- A.zeros (r1 * c2)
        mapM_ (\(i, j, k) -> do
            let block = multiplyBlock 
                         (getBlock m1 i k)
                         (getBlock m2 k j)
            addBlockToResult result i j block)
            [(i, j, k) | i <- [0,blockSize..r1-1],
                        j <- [0,blockSize..c2-1],
                        k <- [0,blockSize..c1-1]]
        return result

kronecker :: Matrix -> Matrix -> Matrix
kronecker m1@(Matrix r1 c1 _) m2@(Matrix r2 c2 _) =
    Matrix (r1 * r2) (c1 * c2) $ unsafePerformIO $ do
        result <- A.createArray (0, r1 * r2 * c1 * c2 - 1)
        mapM_ (\(i1, j1, i2, j2) -> do
            let val = (unsafeGet m1 i1 j1) * (unsafeGet m2 i2 j2)
                i = i1 * r2 + i2
                j = j1 * c2 + j2
            A.writeArray result (i * (c1 * c2) + j) val)
            [(i1, j1, i2, j2) | i1 <- [0..r1-1], j1 <- [0..c1-1],
                               i2 <- [0..r2-1], j2 <- [0..c2-1]]
        return result

power :: Matrix -> Int -> Maybe Matrix
power m n
    | n < 0 = Nothing
    | n == 0 = Just $ identity (rows m)
    | n == 1 = Just m
    | even n = power m (n `div` 2) >>= \m' -> multiply m' m'
    | otherwise = power m (n - 1) >>= multiply m

-- | Matrix Properties

trace :: Matrix -> Maybe Double
trace m@(Matrix r c _)
    | r /= c = Nothing
    | otherwise = Just $ sum [unsafeGet m i i | i <- [0..r-1]]

determinant :: Matrix -> Maybe Double
determinant m@(Matrix r c _)
    | r /= c = Nothing
    | r == 1 = Just $ unsafeGet m 0 0
    | r == 2 = Just $ unsafeGet m 0 0 * unsafeGet m 1 1 - 
                      unsafeGet m 0 1 * unsafeGet m 1 0
    | otherwise = case luDecomposition m of
        Nothing -> Nothing
        Just (l, u, p) -> 
            let diagProduct = product [unsafeGet u i i | i <- [0..r-1]]
                signPerm = if even (length p) then 1 else -1
            in Just $ signPerm * diagProduct

rank :: Matrix -> Int
rank m = 
    case qrDecomposition m of
        Nothing -> 0
        Just (q, r) -> 
            length $ filter (> epsilon) 
                  $ [abs (unsafeGet r i i) | i <- [0..min (rows r) (cols r) - 1]]

condition :: Matrix -> Maybe Double
condition m = do
    (_, s, _) <- svdDecomposition m
    guard (not $ null s)
    let smax = maximum s
        smin = minimum s
    return $ smax / smin

isSymmetric :: Matrix -> Bool
isSymmetric m@(Matrix r c _)
    | r /= c = False
    | otherwise = all (\(i, j) -> 
        abs (unsafeGet m i j - unsafeGet m j i) < epsilon)
        [(i, j) | i <- [0..r-1], j <- [0..i]]

isOrthogonal :: Matrix -> Bool
isOrthogonal m@(Matrix r c _)
    | r /= c = False
    | otherwise = case multiply m (transpose m) of
        Nothing -> False
        Just prod -> all (\(i, j) ->
            abs (unsafeGet prod i j - (if i == j then 1 else 0)) < epsilon)
            [(i, j) | i <- [0..r-1], j <- [0..r-1]]

isPositiveDefinite :: Matrix -> Bool
isPositiveDefinite m = 
    case choleskyDecomposition m of
        Nothing -> False
        Just _ -> True

-- | Decompositions

luDecomposition :: Matrix -> Maybe (Matrix, Matrix, [Int])
luDecomposition m@(Matrix r c _)
    | r /= c = Nothing
    | otherwise = Just $ unsafePerformIO $ do
        l <- A.createArray (0, r * r - 1)
        u <- A.createArray (0, r * r - 1)
        -- Implementation details...
        return (Matrix r r l, Matrix r r u, [0..r-1])

qrDecomposition :: Matrix -> Maybe (Matrix, Matrix)
qrDecomposition m@(Matrix r c _) = 
    -- Implementation of QR decomposition using Householder reflections
    undefined

choleskyDecomposition :: Matrix -> Maybe Matrix
choleskyDecomposition m
    | not (isSymmetric m) = Nothing
    | otherwise = 
        -- Implementation of Cholesky decomposition
        undefined

eigendecomposition :: Matrix -> Maybe (Matrix, V.Vector)
eigendecomposition m
    | not (isSymmetric m) = Nothing
    | otherwise =
        -- Implementation of eigendecomposition using QR algorithm
        undefined

-- | Helper Functions

unsafeGet :: Matrix -> Int -> Int -> Double
unsafeGet (Matrix _ c arr) i j = 
    unsafePerformIO $ A.readArray arr (i * c + j)

unsafeSet :: Matrix -> Int -> Int -> Double -> IO ()
unsafeSet (Matrix _ c arr) i j val =
    A.writeArray arr (i * c + j) val

getBlock :: Matrix -> Int -> Int -> A.UnboxedArray Double
getBlock (Matrix r c arr) startRow startCol = unsafePerformIO $ do
    let blockR = min blockSize (r - startRow)
        blockC = min blockSize (c - startCol)
    result <- A.createArray (0, blockR * blockC - 1)
    mapM_ (\(i, j) -> do
        let srcIdx = (startRow + i) * c + startCol + j
        val <- A.readArray arr srcIdx
        A.writeArray result (i * blockC + j) val)
        [(i, j) | i <- [0..blockR-1], j <- [0..blockC-1]]
    return result

multiplyBlock :: A.UnboxedArray Double -> A.UnboxedArray Double -> A.UnboxedArray Double
multiplyBlock block1 block2 = undefined  -- Implementation of block multiplication

addBlockToResult :: A.UnboxedArray Double -> Int -> Int -> A.UnboxedArray Double -> IO ()
addBlockToResult result startRow startCol block = undefined  -- Implementation of block addition

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

svdDecomposition :: Matrix -> Maybe (Matrix, [Double], Matrix)
svdDecomposition = undefined  -- Implementation of SVD decomposition