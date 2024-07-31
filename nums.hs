type Matrix = [[a]]

transpose :: Matrix a -> Matrix a
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x) 

dotProduct :: Num a => [a] -> [a] -> a
dotProduct xs ys = sum $ zipWith (*) xs ys

matMul = :: Num a => Matrix a -> Matrix a -> Maybe (Matrix a)
matMul a b = 
    if numColsA /= numRowsB 
    then Nothing 
    else Just [[dotProduct row col | col <- transpose b] | row <- a]
  where
    numColsA = length (head a)
    numRowsB = length b
