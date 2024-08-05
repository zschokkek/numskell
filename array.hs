data Array i e = Array {
    bounds :: (i, i),
    elements :: [e]
} deriving (Show, Eq)

-- Create an array from a list and bounds
listArray :: (Ix i) => (i, i) -> [e] -> Array i e
listArray bnds elems
    | rangeSize bnds == length elems = Array bnds elems
    | otherwise = error "List length does not match array bounds"

-- Create an array with all elements initialized to the same value
array :: (Ix i) => (i, i) -> [(i, e)] -> Array i e
array bnds assocs = Array bnds (map (lookupDefault def assocs) (range bnds))
  where
    def = error "Uninitialized element"
    lookupDefault d as k = case lookup k as of
        Just v -> v
        Nothing -> d

-- Indexing with bounds checking
(!) :: (Ix i) => Array i e -> i -> e
(Array bnds elems) ! idx
    | inRange bnds idx = elems !! index bnds idx
    | otherwise = error "Index out of bounds"

-- Update elements in the array
(//) :: (Ix i) => Array i e -> [(i, e)] -> Array i e
(Array bnds elems) // updates = Array bnds (go elems updates)
  where
    go es [] = es
    go es ((i, e):us) = go (take idx es ++ [e] ++ drop (idx + 1) es) us
      where
        idx = index bnds i


data Array i e = Array {
    bounds :: (i, i),
    elements :: [e]
} deriving (Show, Eq)

-- Create an array from a list and bounds
listArray :: (Ix i) => (i, i) -> [e] -> Array i e
listArray bnds elems
    | rangeSize bnds == length elems = Array bnds elems
    | otherwise = error "List length does not match array bounds"

-- Create an array with all elements initialized to the same value
array :: (Ix i) => (i, i) -> [(i, e)] -> Array i e
array bnds assocs = Array bnds (map (lookupDefault def assocs) (range bnds))
  where
    def = error "Uninitialized element"
    lookupDefault d as k = case lookup k as of
        Just v -> v
        Nothing -> d

-- Indexing with bounds checking
(!) :: (Ix i) => Array i e -> i -> e
(Array bnds elems) ! idx
    | inRange bnds idx = elems !! index bnds idx
    | otherwise = error "Index out of bounds"

-- Update elements in the array
(//) :: (Ix i) => Array i e -> [(i, e)] -> Array i e
(Array bnds elems) // updates = Array bnds (go elems updates)
  where
    go es [] = es
    go es ((i, e):us) = go (take idx es ++ [e] ++ drop (idx + 1) es) us
      where
        idx = index bnds i

-- Range size
rangeSize :: (Ix i) => (i, i) -> Int
rangeSize bnds = length (range bnds)

-- Convert an index to a position in the list
index :: (Ix i) => (i, i) -> i -> Int
index bnds idx = fromJust (elemIndex idx (range bnds))
  where
    fromJust (Just x) = x
    fromJust Nothing = error "Index not found"

-- Generate a range of indices
range :: (Ix i) => (i, i) -> [i]
range (low, high) = [low..high]

-- Check if an index is within the range
inRange :: (Ord i) => (i, i) -> i -> Bool
inRange (low, high) idx = low <= idx && idx <= high