-----------TYPEDEFS-----------
type Vector = [Double]
type Matrix = [Vector]

blockSize :: Int
blockSize = 64  -- Block size can be tuned based on the cache size of your machine

-----------PRINT-----------
--print Matrix
printMatrix :: Matrix -> IO ()
printMatrix [] = return ()
printMatrix (r:rs) = do
    print r
    printMatrix rs

-----------HELPER FUNCTIONS-----------
--helper to get indentity matrix 
identityMatrix :: Int -> Matrix
identityMatrix n = [ [fromIntegral $ fromEnum $ i == j | j <- [0..n-1]] | i <- [0..n-1] ]

subtractLambdaI :: Double -> Matrix -> Matrix
subtractLambdaI lambda mat = 
    zipWith (zipWith (\a b -> a - lambda * b)) mat (identityMatrix (length mat))

--check if identity
isIdentity :: Matrix -> Bool
isIdentity mat = all (\(i, row) -> row == [fromIntegral $ fromEnum $ i == j | j <- [0..length row - 1]]) (zip [0..] mat)

--transpose fn
transposeMatrix :: Matrix -> Matrix
transposeMatrix ([]:_) = []
transposeMatrix x = (map head x) : transposeMatrix (map tail x)

--swap rows in matrix
swapRows :: Matrix -> Int -> Int -> Matrix
swapRows mat i j = 
    let rowI = mat !! i
        rowJ = mat !! j
    in [ if k == i then rowJ else if k == j then rowI else mat !! k | k <- [0..length mat - 1] ]

--helper fn for inverse 
findNonZeroAndSwap :: Matrix -> Int -> Int -> Matrix
findNonZeroAndSwap mat i j =
    case filter (\k -> mat !! k !! j /= 0) [i+1..length mat - 1] of
        [] -> mat
        (k:_) -> swapRows mat i k

-- clockwise rotate 90
rotateMatrix:: Matrix -> Matrix
rotateMatrix = transposeMatrix . map reverse

-- counterclockwise rotate 90
rotateMatrixCounter :: Matrix -> Matrix
rotateMatrixCounter = map reverse . transposeMatrix
-----------SCALAR OPERATIONS-----------
scalarMul :: Double -> Matrix -> Matrix
scalarMul k = map (map (k *))


--scale rows in matrix
scaleRow :: Matrix -> Int -> Double -> Matrix
scaleRow mat i scale = 
    let rowI = mat !! i
    in [ if k == i then map (* scale) rowI else mat !! k | k <- [0..length mat - 1] ]

--add scaled row 
addScaledRow :: Matrix -> Int -> Int -> Double -> Matrix
addScaledRow mat i j scale = 
    let rowI = mat !! i
        rowJ = mat !! j
    in [ if k == j then zipWith (+) rowJ (map (* scale) rowI) else mat !! k | k <- [0..length mat - 1] ]

-----------MATRIX INVERSION-----------
--get minor arguably not helper 
minor :: Int -> Int -> Matrix -> Matrix
minor i j mat = [ [ mat !! r !! c | c <- [0..length (head mat) - 1], c /= j ]
                | r <- [0..length mat - 1], r /= i ]

--co factor 
cofactor :: Int -> Int -> Matrix -> Double
cofactor i j mat = ((-1) ^ (i + j)) * determinant (minor i j mat)

-- Frobenius norm for matrices
frobeniusNorm :: Matrix -> Double
frobeniusNorm mat = sqrt . sum $ concatMap (map (^2)) mat

inverse :: Matrix -> Maybe Matrix
inverse mat = 
    let n = length mat
        augmented = augmentWithIdentity mat
        reduced = gaussJordan augmented n 0
    in if isIdentity (map (take n) (drop n <$> reduced))
       then Just (map (drop n) reduced)
       else Nothing

augmentWithIdentity :: Matrix -> Matrix
augmentWithIdentity mat = zipWith (++) mat (identityMatrix (length mat))

gaussJordan :: Matrix -> Int -> Int -> Matrix
gaussJordan mat n i
    | i >= n = mat
    | otherwise =
        let mat' = if mat !! i !! i == 0 
                   then findNonZeroAndSwap mat i i
                   else mat
            scale = 1 / (mat' !! i !! i)
            mat'' = scaleRow mat' i scale
            mat''' = foldl (\acc j -> if j /= i 
                                      then addScaledRow acc i j (- (acc !! j !! i))
                                      else acc) mat'' [0..n-1]
        in gaussJordan mat''' n (i + 1)

-----------BLOCKING-----------
--get submatrix blocking for matMul optimization
getBlock :: Matrix -> Int -> Int -> Int -> Int -> Matrix
getBlock mat rowStart rowEnd colStart colEnd =
    [ take (colEnd - colStart + 1) . drop colStart $ row | row <- take (rowEnd - rowStart + 1) . drop rowStart $ mat ]

setBlock :: Matrix -> Int -> Int -> Matrix -> Matrix
setBlock mat rowStart colStart block =
    [ if i >= rowStart && i < rowStart + length block
      then take colStart row ++ block !! (i - rowStart) ++ drop (colStart + length (head block)) row
      else row
    | (i, row) <- zip [0..] mat ]

multiplyBlocks :: Matrix -> Matrix -> Matrix
multiplyBlocks a b =
    let transposedB = transposeMatrix b
    in [[ dotProduct ar bc | bc <- transposedB ] | ar <- a]

-- Function to add two blocks
addBlocks :: Matrix -> Matrix -> Matrix
addBlocks a b = zipWith (zipWith (+)) a b


-----------MATRIX MATH-----------

outerProduct :: Vector -> Vector -> Matrix
outerProduct u v = [[x * y | y <- v] | x <- u]

dotProduct :: Vector -> Vector -> Double
dotProduct xs ys = sum $ zipWith (*) xs ys

-- Optimized dot product using blocking
fastProductBlock :: Vector -> Vector -> Int -> Double
fastProductBlock xs ys blockSize = sum [sum $ zipWith (*) (take blockSize $ drop i xs) (take blockSize $ drop i ys) | i <- [0, blockSize .. length xs - 1]]

tensorProduct :: Matrix -> Matrix -> Matrix
tensorProduct a b = concat [[ [x * y | y <- rowB] | rowB <- b ] | rowA <- a, x <- rowA]

--optimzied matMul
matMul :: Matrix -> Matrix -> Matrix
matMul a b =
    let n = length a
        m = length (head a)
        p = length (head b)
        c = replicate n (replicate p 0)
    in foldl (\acc (i, j, k) ->
                 let aBlock = getBlock a i (min (i + blockSize - 1) (n - 1)) k (min (k + blockSize - 1) (m - 1))
                     bBlock = getBlock b k (min (k + blockSize - 1) (m - 1)) j (min (j + blockSize - 1) (p - 1))
                     cBlock = getBlock acc i (min (i + blockSize - 1) (n - 1)) j (min (j + blockSize - 1) (p - 1))
                     productBlock = multiplyBlocks aBlock bBlock
                 in setBlock acc i j (addBlocks cBlock productBlock)
              ) c [(i, j, k) | i <- [0, blockSize .. n-1], j <- [0, blockSize .. p-1], k <- [0, blockSize .. m-1]]

-- Matrix subtraction TODO: OPTIMIZE
matSub :: Matrix -> Matrix -> Matrix
matSub a b = zipWith (zipWith (-)) a b

--optimized matSum
matSum :: Matrix -> Matrix -> Maybe Matrix
matSum a b =
    if dimensions a /= dimensions b
    then Nothing
    else let n = length a
             m = length (head a)
             c = replicate n (replicate m 0)
         in Just $ foldl (\acc (i, j) ->
                            let aBlock = getBlock a i (min (i + blockSize - 1) (n - 1)) j (min (j + blockSize - 1) (m - 1))
                                bBlock = getBlock b i (min (i + blockSize - 1) (n - 1)) j (min (j + blockSize - 1) (m - 1))
                                sumBlock = addBlocks aBlock bBlock
                            in setBlock acc i j sumBlock
                         ) c [(i, j) | i <- [0, blockSize .. n-1], j <- [0, blockSize .. m-1]]
  where
    dimensions m = (length m, length (head m))


determinant :: Matrix -> Double
determinant [[a]] = a -- Base case for 1x1 matrix
determinant mat = sum [ (mat !! 0 !! j) * cofactor 0 j mat | j <- [0..length (head mat) - 1] ]

-----------EIGENVALUE CALCULATION-----------
-- Calculate the norm of a vector
vectorNorm :: Vector -> Double
vectorNorm v = sqrt (sum (map (^2) v))

-- Normalize a vector
normalize :: Vector -> Vector
normalize v = let norm = vectorNorm v in map (/ norm) v

-- Matrix-vector multiplication
matVecMul :: Matrix -> Vector -> Vector
matVecMul mat vec = map (dotProduct vec) mat

-----------MISC-----------

luDecomposition :: Matrix -> (Matrix, Matrix)
luDecomposition mat = (lower, upper)
  where
    n = length mat
    upper = [[if i <= j then mat !! i !! j - sum [lower !! i !! k * upper !! k !! j | k <- [0..i-1]] else 0 | j <- [0..n-1]] | i <- [0..n-1]]
    lower = [[if i > j then (mat !! i !! j - sum [lower !! i !! k * upper !! k !! j | k <- [0..j-1]]) / upper !! j !! j else if i == j then 1 else 0 | j <- [0..n-1]] | i <- [0..n-1]]


-----------MAIN-----------
main :: IO ()
main = do
    let matrixA = [[1, -1, -8, -13, 1], [5, 3, 32, -17, 21]]
    printMatrix matrixA
    let matrixB = gaussJordan matrixA 2 0
    printMatrix matrixB
   
