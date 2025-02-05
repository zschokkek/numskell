{-|
Module      : Array
Description : Low-level array operations with direct memory management
Copyright   : (c) Kyle Zschokke, 2025
License     : BSD3
Stability   : experimental

This module provides the foundational array operations that the higher-level
mathematical operations will build upon. It handles direct memory management
through the Foreign module while providing a safe interface through the IO monad.
-}

module Array 
    ( -- * Types
      UnboxedArray(..)
      -- * Array Creation
    , createArray
    , fromList
    , zeros
    , ones
      -- * Memory Management
    , freeArray
    , withArray
      -- * Array Access
    , readArray
    , writeArray
    , (!)
    , (//)
      -- * Array Information
    , bounds
    , size
      -- * Array Conversion
    , toList
    ) where

import Foreign
import Foreign.C.Types
import Control.Exception (bracket)
import System.IO.Unsafe (unsafePerformIO)

-- | An array with direct memory management
data UnboxedArray a = UnboxedArray {
    bounds :: (Int, Int),  -- ^ Inclusive bounds of the array
    ptr    :: Ptr a        -- ^ Raw memory pointer
} deriving Eq

-- | Show instance for debugging and REPL use
instance (Show a, Storable a) => Show (UnboxedArray a) where
    show arr = "UnboxedArray " ++ show (bounds arr) ++ ": " ++ show (toList arr)

-- | Create a new array with specified bounds
createArray :: Storable a => (Int, Int) -> IO (UnboxedArray a)
createArray bnds@(l, u) = do
    let size = u - l + 1
    if size <= 0 
        then error "Array size must be positive"
        else do
            ptr <- mallocArray size
            return $ UnboxedArray bnds ptr

-- | Free the memory associated with an array
freeArray :: Storable a => UnboxedArray a -> IO ()
freeArray (UnboxedArray _ ptr) = free ptr

-- | Safely create and use an array within a given scope
withArray :: Storable a => (Int, Int) -> (UnboxedArray a -> IO b) -> IO b
withArray bnds = bracket (createArray bnds) freeArray

-- | Read a value from the array
readArray :: Storable a => UnboxedArray a -> Int -> IO a
readArray (UnboxedArray (l, u) ptr) i 
    | i < l || i > u = error "Array index out of bounds"
    | otherwise = peekElemOff ptr (i - l)

-- | Write a value to the array
writeArray :: Storable a => UnboxedArray a -> Int -> a -> IO ()
writeArray (UnboxedArray (l, u) ptr) i val
    | i < l || i > u = error "Array index out of bounds"
    | otherwise = pokeElemOff ptr (i - l) val

-- | Create an array from a list
fromList :: Storable a => (Int, Int) -> [a] -> IO (UnboxedArray a)
fromList bnds@(l, u) xs = do
    let expectedSize = u - l + 1
    if length xs /= expectedSize
        then error "List length does not match array bounds"
        else do
            arr <- createArray bnds
            mapM_ (\(i, x) -> writeArray arr i x) $ zip [l..u] xs
            return arr

-- | Convert an array to a list
toList :: Storable a => UnboxedArray a -> [a]
toList arr@(UnboxedArray (l, u) _) = 
    map (unsafePerformIO . readArray arr) [l..u]

-- | Get the size of the array
size :: UnboxedArray a -> Int
size (UnboxedArray (l, u) _) = u - l + 1

-- | Create an array filled with zeros
zeros :: Int -> IO (UnboxedArray Double)
zeros n = fromList (0, n-1) (replicate n 0.0)

-- | Create an array filled with ones
ones :: Int -> IO (UnboxedArray Double)
ones n = fromList (0, n-1) (replicate n 1.0)

-- | Array index operator
(!) :: Storable a => UnboxedArray a -> Int -> IO a
(!) = readArray

-- | Array update operator
(//) :: Storable a => UnboxedArray a -> [(Int, a)] -> IO ()
(//) arr updates = mapM_ (uncurry (writeArray arr)) updates

-- | Helper function to check if an index is in bounds
inBounds :: UnboxedArray a -> Int -> Bool
inBounds (UnboxedArray (l, u) _) i = i >= l && i <= u