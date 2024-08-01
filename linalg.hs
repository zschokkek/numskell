--TODO: integrate calculus? tensors? 

type Matrix a = [[a]]
type Vector a = [a]

printMatrix :: Show a => Matrix a -> IO ()
printMatrix [] = return ()
printMatrix (r:rs) = do
    print r
    printMatrix rs

identityMatrix :: Num a => Int -> Matrix a
identityMatrix n = [ [fromIntegral $ fromEnum $ i == j | j <- [0..n-1]] | i <- [0..n-1] ]

transpose :: Matrix a -> Matrix a
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

dotProduct :: Num a => [a] -> [a] -> a
dotProduct xs ys = sum $ zipWith (*) xs ys

matMul :: Num a => Matrix a -> Matrix a -> Matrix a
matMul a b = [[ sum $ zipWith (*) ar bc | bc <- transpose b ] | ar <- a]
  where
    transpose :: Matrix a -> Matrix a
    transpose ([]:_) = []
    transpose x = (map head x) : transpose (map tail x)

matSum :: Num a => Matrix a -> Matrix a -> Maybe (Matrix a)
matSum a b = 
    if dimensions a /= dimensions b
    then Nothing
    else Just (zipWith (zipWith (+)) a b)
  where
    dimensions m = (length m, length (head m))

scalarMul :: Num a => a -> Matrix a -> Matrix a
scalarMul k = map (map (k *))

tensorProduct :: Num a => Matrix a -> Matrix a -> Matrix a
tensorProduct a b = concat [[ [x * y | y <- rowB] | rowB <- b ] | rowA <- a, x <- rowA]


minor :: Int -> Int -> Matrix a -> Matrix a
minor i j mat = [ [ mat !! r !! c | c <- [0..length (head mat) - 1], c /= j ]
                | r <- [0..length mat - 1], r /= i ]

cofactor :: Num a => Int -> Int -> Matrix a -> a
cofactor i j mat = ((-1) ^ (i + j)) * determinant (minor i j mat)

determinant :: Num a => Matrix a -> a
determinant [[a]] = a -- Base case for 1x1 matrix
determinant mat = sum [ (mat !! 0 !! j) * cofactor 0 j mat | j <- [0..length (head mat) - 1] ]

main :: IO ()
main = do
    let matrixA = [[1, 2, 3], [3, 2, 1]] :: Matrix Double
    let matrixB = [[4, 3, 2], [2, 4, 3], [1, 2, 1]] :: Matrix Double
    
    putStrLn "Matrix Multiplication (matmul):"
    let resultMatMul = matMul matrixA matrixB
    printMatrix resultMatMul

    putStrLn "\nTensor Product:"
    let resultTensorProduct = tensorProduct matrixA matrixB
    printMatrix resultTensorProduct