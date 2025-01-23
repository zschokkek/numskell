{-# LANGUAGE ScopedTypeVariables #-}

import Criterion.Main
import Foreign
import Foreign.C.Types
import Control.DeepSeq
import Control.Exception (bracket)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Array as A
import qualified Data.Array.Unboxed as U

data UnboxedArray a = UnboxedArray {
    bounds :: (Int, Int),
    ptr    :: Ptr a
} deriving (Show, Eq)

instance (Storable a, NFData a) => NFData (UnboxedArray a) where
    rnf (UnboxedArray bnds ptr) = rnf bnds `seq` deepseqArray ptr
      where
        deepseqArray p = unsafePerformIO $ do
            let (l, u) = bnds
                size = u - l + 1
            elements <- peekArray size p
            return $ rnf elements

-- Create an unboxed array with a specific number of elements
createArray :: Storable a => (Int, Int) -> IO (UnboxedArray a)
createArray bnds@(l, u) = do
    let size = (u - l + 1)
    ptr <- mallocArray size
    return $ UnboxedArray bnds ptr

-- Free the unboxed array
freeArray :: Storable a => UnboxedArray a -> IO ()
freeArray (UnboxedArray _ ptr) = free ptr

-- Helper function to safely create and use an array
withArray :: Storable a => (Int, Int) -> (UnboxedArray a -> IO b) -> IO b
withArray bnds = bracket (createArray bnds) freeArray

-- Read an element from the unboxed array
readArray :: Storable a => UnboxedArray a -> Int -> IO a
readArray (UnboxedArray (l, _) ptr) i = do
    let idx = i - l
    peekElemOff ptr idx

-- Write an element to the unboxed array
writeArray :: Storable a => UnboxedArray a -> Int -> a -> IO ()
writeArray (UnboxedArray (l, _) ptr) i val = do
    let idx = i - l
    pokeElemOff ptr idx val

-- Initialize an unboxed array from a list
fromList :: Storable a => (Int, Int) -> [a] -> IO (UnboxedArray a)
fromList bnds@(l, u) xs = do
    arr <- createArray bnds
    let size = (u - l + 1)
    if length xs /= size
        then error "List length does not match array bounds"
        else do
            let fill i [] = return ()
                fill i (x:xs) = pokeElemOff (ptr arr) i x >> fill (i + 1) xs
            fill 0 xs
            return arr

-- Convert an unboxed array to a list
toList :: (Storable a) => UnboxedArray a -> [a]
toList arr = map (unsafePerformIO . readArray arr) [l..u]
    where (l, u) = bounds arr

-- Safe indexing
(!) :: (Storable a) => UnboxedArray a -> Int -> IO a
(!) = readArray

-- Updating elements
(//) :: (Storable a) => UnboxedArray a -> [(Int, a)] -> IO ()
(//) arr updates = mapM_ (uncurry (writeArray arr)) updates

-- Benchmark comparisons
main :: IO ()
main = do
    let size = 1000000
    let bnds = (0, size - 1)
    let values = [1..size] :: [Int]
    
    -- Create default boxed array
    let defaultArray :: A.Array Int Int = A.listArray bnds values
    
    -- Create default unboxed array
    let defaultUArray :: U.UArray Int Int = U.listArray bnds values
    
    defaultMain [
        bgroup "Array Comparison" [
            bgroup "UnboxedArray" [
                bench "creation from list" $ whnfIO (fromList bnds values),
                env (fromList bnds values) $ \arr -> bgroup "operations" [
                    bench "indexing" $ whnfIO (arr ! (size `div` 2)),
                    bench "updating" $ whnfIO (arr // [(size `div` 2, 42)]),
                    bench "toList" $ whnf (toList arr)
                ]
            ],
            bgroup "Data.Array" [
                bench "creation from list" $ whnf (A.listArray bnds) values,
                bench "indexing" $ whnf (defaultArray A.!) (size `div` 2),
                bench "updating" $ whnf ((A.elems (defaultArray A.// [(size `div` 2, 42)]) !!) (size `div` 2)),
                bench "toList" $ whnf A.elems defaultArray
            ],
            bgroup "Data.Array.Unboxed" [
                bench "creation from list" $ whnf (U.listArray bnds) values,
                bench "indexing" $ whnf (defaultUArray U.!) (size `div` 2),
                bench "updating" $ whnf ((U.elems (defaultUArray U.// [(size `div` 2, 42)]) !!) (size `div` 2)),
                bench "toList" $ whnf U.elems defaultUArray
            ]
        ]
        ]
