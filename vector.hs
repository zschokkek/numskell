module Vector (
    Vector,
    fromList,
    toList,
    (!),
    (//),
    mzip,
    mzipWith,
    munzip
) where

-- Indexing
(!) :: Vector a -> Int -> a
(Vector xs) ! i = xs !! i

-- Updating
(//) :: Vector a -> [(Int, a)] -> Vector a
(Vector xs) // updates = Vector (go xs updates)
    where
        go xs [] = xs
        go xs ((i, x):us) = go (take i xs ++ [x] ++ drop (i + 1) xs) us

instance Functor Vector where
    fmap f (Vector xs) = Vector (map f xs)

instance Applicative Vector where
    pure x = Vector (repeat x)
    (Vector fs) <*> (Vector xs) = Vector (zipWith ($) fs xs)

instance Monad Vector where
    return = pure
    (Vector xs) >>= f = Vector (concatMap (toList . f) xs)

class Monad m => MonadZip m where
    mzip :: m a -> m b -> m (a, b)
    mzipWith :: (a -> b -> c) -> m a -> m b -> m c
    munzip :: m (a, b) -> (m a, m b)

instance MonadZip Vector where
    mzip (Vector xs) (Vector ys) = Vector (zip xs ys)
    mzipWith f (Vector xs) (Vector ys) = Vector (zipWith f xs ys)
    munzip (Vector xys) = (Vector (map fst xys), Vector (map snd xys))

-- Create a vector with n elements, all initialized to the same value
replicate :: Int -> a -> Vector a
replicate n x = Vector (Prelude.replicate n x)

-- Create a vector with elements generated by a function
generate :: Int -> (Int -> a) -> Vector a
generate n f = Vector (map f [0..n-1])

-- Sum all elements in a vector
sum :: Num a => Vector a -> a
sum (Vector xs) = foldl (+) 0 xs

-- Zip two vectors together
zip :: Vector a -> Vector b -> Vector (a, b)
zip (Vector xs) (Vector ys) = Vector (Prelude.zip xs ys)


instance Foldable Vector where
    foldr f z (Vector xs) = foldr f z xs

instance Traversable Vector where
    traverse f (Vector xs) = Vector <$> traverse f xs

instance MonadFix Vector where
    mfix f = Vector $ fix $ toList . f . head . toList