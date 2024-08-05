type Vector = [Double]
type Matrix = [Vector]

dotProduct :: Vector -> Vector -> Double
dotProduct v1 v2 = sum $ zipWith (*) v1 v2

norm :: Vector -> Double
norm v = sqrt $ dotProduct v v

scale :: Double -> Vector -> Vector
scale k = map (k *)

add :: Vector -> Vector -> Vector
add = zipWith (+)

subtractVec :: Vector -> Vector -> Vector
subtractVec = zipWith (-)

project :: Vector -> Vector -> Vector
project u v = scale ((dotProduct v u) / (dotProduct u u)) u

gramSchmidt :: [Vector] -> [Vector]
gramSchmidt = gramSchmidt' []
  where
    gramSchmidt' :: [Vector] -> [Vector] -> [Vector]
    gramSchmidt' qs [] = reverse qs
    gramSchmidt' qs (v:vs) = gramSchmidt' (q:qs) vs
      where
        q = normalize $ foldl subtractVec v (map (`project` v) qs)
        normalize u = scale (1 / norm u) u

qrDecomposition :: Matrix -> (Matrix, Matrix)
qrDecomposition a = (transpose q, r)
  where
    q = gramSchmidt $ transpose a
    qt = transpose q
    r = [[if i <= j then dotProduct (q !! i) (transpose a !! j) else 0 | j <- [0..length a - 1]] | i <- [0..length a - 1]]

matrixMultiply :: Matrix -> Matrix -> Matrix
matrixMultiply a b = [[sum $ zipWith (*) ar bc | bc <- transpose b] | ar <- a]

qrAlgorithm :: Matrix -> Int -> Matrix
qrAlgorithm a n = iterate qrStep a !! n
  where
    qrStep a' = let (q, r) = qrDecomposition a' in matrixMultiply r q


extractEigenvalues :: Matrix -> [Double]
extractEigenvalues a = [a !! i !! i | i <- [0..length a - 1]]

transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)


main :: IO ()
main = do
    let a = [[4, 1, 1], [1, 3, 1], [1, 1, 2]]
        n = 100  -- number of iterations
        result = qrAlgorithm a n
        eigenvalues = extractEigenvalues result
    putStrLn "Resulting matrix after QR Algorithm:"
    mapM_ print result
    putStrLn "Eigenvalues:"
    print eigenvalues