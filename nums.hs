type Matrix a = [[a]]

identityMatrix :: Num a => Int -> Matrix a
identityMatrix n = [ [fromIntegral $ fromEnum $ i == j | j <- [1..n]] | i <- [1..n] ]


transpose :: Matrix a -> Matrix a
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x) 

dotProduct :: Num a => [a] -> [a] -> a
dotProduct xs ys = sum $ zipWith (*) xs ys

matMul :: Num a => Matrix a -> Matrix a -> Maybe (Matrix a)
matMul a b = 
    if numColsA /= numRowsB 
    then Nothing 
    else Just [[dotProduct row col | col <- transpose b] | row <- a]
  where
    numColsA = length (head a)
    numRowsB = length b

matSum:: Num a => Matrix a -> Matrix a -> Maybe (Matrix a)
matSum a b = 
    if dimensions a /= dimensions b
    then Nothing
    else Just (zipWith (zipWith (+)) a b)
  where
    dimensions m = (length m, length (head m))

scalarMul :: Num a => a -> Matrix a -> Matrix a
scalarMul k = map (map (k *))

#Generalize? 
determinant2x2 :: Num a => Matrix a -> Maybe a      
determinant2x2 [[a, b], [c, d]] = Just (a * d - b * c)      
determinant2x2 _ = Nothing

-- Example matrices
matrixA :: Matrix Int
matrixA = [[1, 2, 3], 
           [4, 5, 6]]

matrixB :: Matrix Int
matrixB = [[7, 8], 
           [9, 10], 
           [11, 12]]

-- Multiply the matrices
main :: IO ()
main = do
    let result = matMul matrixA matrixB
    case result of
        Just matrixC -> print matrixC
        Nothing -> putStrLn "Matrix dimensions do not match for multiplication"