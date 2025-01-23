-- array.hs
module MyArray where
import Foreign
import Foreign.C.Types

data UnboxedArray a = UnboxedArray {
    bounds :: (Int, Int),
    ptr    :: Ptr a
}

instance (Show a, Storable a) => Show (UnboxedArray a) where
    show arr = show (bounds arr) ++ ": " ++ show (toList arr)

import Control.Exception (bracket)

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

(!) :: (Storable a) => UnboxedArray a -> Int -> IO a
(!) = readArray

(//) :: (Storable a) => UnboxedArray a -> [(Int, a)] -> IO ()
(//) arr updates = mapM_ (uncurry (writeArray arr)) updates

