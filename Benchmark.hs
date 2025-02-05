{-|
Module      : Benchmark.Implementation
Description : Implementation of benchmark helper functions
Copyright   : (c) Kyle Zschokke, 2025
License     : BSD3
-}

-- | Test Data Generation Functions

-- | Generate a test matrix of given size with controlled properties
generateTestMatrix :: Int -> M.Matrix
generateTestMatrix n = 
    -- We create a matrix with known properties for reliable testing
    let diagonal = [1.0 + sin (fromIntegral i) | i <- [1..n]]
        rotation = generateRotationMatrix n (pi/4)  -- 45-degree rotation
    in case (M.diagonal (V.fromList diagonal), rotation) of
        (diag, rot) -> case M.multiply rot (M.multiply diag (M.transpose rot)) of
            Just m -> m
            Nothing -> error "Matrix generation failed"

-- | Generate a rotation matrix for given dimension and angle
generateRotationMatrix :: Int -> Double -> M.Matrix
generateRotationMatrix n theta =
    let indices = [(i, j) | i <- [0..n-1], j <- [0..n-1]]
        elements = [rotationElement i j | (i, j) <- indices]
    in M.fromLists (chunksOf n elements)
  where
    rotationElement i j
        | i == j = cos theta
        | i == j + 1 = sin theta
        | i + 1 == j = -sin theta
        | otherwise = 0.0

-- | Generate regression data with known coefficients and controlled noise
generateRegressionData :: Int -> (M.Matrix, V.Vector)
generateRegressionData n = 
    -- Create predictors with known correlation structure
    let x = M.fromLists [[1.0, genPredictor i j | j <- [1..5]] | i <- [1..n]]
        -- True coefficients
        beta = V.fromList [1.0, 2.0, -0.5, 1.5, -1.0, 0.8]
        -- Generate response with controlled noise
        epsilon = V.fromList [generateNormal 0.0 0.1 (fromIntegral i) | i <- [1..n]]
        y = case M.multiply x (M.asColumn beta) of
            Just yPred -> V.add (M.toVector yPred) epsilon
            Nothing -> error "Regression data generation failed"
    in (x, y)
  where
    genPredictor i j = 
        -- Generate predictors with known correlation structure
        sin (fromIntegral i * 0.1) * cos (fromIntegral j * 0.2) +
        generateNormal 0.0 0.1 (fromIntegral (i*j))

-- | Generate an ill-conditioned matrix with specified condition number
generateIllConditionedMatrix :: Double -> M.Matrix
generateIllConditionedMatrix cond = 
    -- We create a matrix with specified condition number using SVD construction
    let n = 100  -- Size of matrix
        -- Generate singular values with specified condition number
        singularValues = V.fromList $ 
            1.0 : [1.0 - (1.0 - 1.0/cond) * (fromIntegral i / fromIntegral n) 
                  | i <- [1..n-1]]
        -- Generate random orthogonal matrices
        u = generateOrthogonalMatrix n
        v = generateOrthogonalMatrix n
        -- Construct matrix with specified singular values
        s = M.diagonal singularValues
    in case (u, v) of
        (Just u', Just v') -> 
            case M.multiply u' (M.multiply s (M.transpose v')) of
                Just m -> m
                Nothing -> error "Ill-conditioned matrix generation failed"
        _ -> error "Orthogonal matrix generation failed"

-- | Generate a random orthogonal matrix using QR decomposition
generateOrthogonalMatrix :: Int -> Maybe M.Matrix
generateOrthogonalMatrix n = 
    let randomMatrix = M.fromLists 
            [[generateNormal 0.0 1.0 (fromIntegral (i*n + j)) 
              | j <- [1..n]] | i <- [1..n]]
    in case LA.qrDecomposition randomMatrix of
        Just (q, _) -> Just q
        Nothing -> Nothing

-- | Measurement Functions

-- | Measure peak memory usage during computation
measurePeakMemory :: NFData a => IO a -> IO (a, Integer)
measurePeakMemory action = do
    -- Get initial memory stats
    initialStats <- getRTSStats
    result <- action
    -- Get final memory stats
    finalStats <- result `deepseq` getRTSStats
    let peakMemory = max_live_bytes finalStats
    return (result, peakMemory)

-- | Measure memory allocation rate
measureAllocationRate :: NFData a => IO a -> IO (a, Double)
measureAllocationRate action = do
    -- Get initial memory stats
    start <- getCPUTime
    initialStats <- getRTSStats
    result <- action
    -- Get final memory stats
    end <- result `deepseq` getCPUTime
    finalStats <- getRTSStats
    
    let totalAllocated = allocated_bytes finalStats - allocated_bytes initialStats
        timeInSeconds = fromIntegral (end - start) / (10^12)
        allocRate = fromIntegral totalAllocated / timeInSeconds
    
    return (result, allocRate)

-- | Analysis Functions

-- | Analyze the tradeoff between numerical accuracy and speed
analyzeAccuracyVsSpeed :: [(Double, Double)] -> String
analyzeAccuracyVsSpeed results = 
    let -- Compute efficiency frontier
        frontier = paretoFrontier results
        -- Generate analysis text
        analysis = "Performance Analysis:\n" ++
                  "---------------------\n" ++
                  "Number of observations: " ++ show (length results) ++ "\n" ++
                  "Efficiency frontier points: " ++ show (length frontier) ++ "\n\n" ++
                  "Key findings:\n" ++
                  formatFindings (analyzeResults results frontier)
    in analysis
  where
    analyzeResults results frontier = 
        let avgAccuracy = sum (map fst results) / fromIntegral (length results)
            avgSpeed = sum (map snd results) / fromIntegral (length results)
            -- Compute various statistics
            findings = [
                "Average accuracy: " ++ printf "%.2e" avgAccuracy,
                "Average speed: " ++ printf "%.2f ms" avgSpeed,
                "Optimal configurations found: " ++ show (length frontier)
                ]
        in findings

-- | Generate comprehensive performance report
generateReport :: [(String, Double, Double)] -> String
generateReport benchResults = 
    let -- Generate different sections of the report
        header = "Performance Benchmark Report\n" ++
                "==========================\n\n"
        
        summary = "Summary Statistics:\n" ++
                 "-----------------\n" ++
                 generateSummaryStats benchResults ++ "\n"
        
        details = "Detailed Results:\n" ++
                 "---------------\n" ++
                 generateDetailedResults benchResults ++ "\n"
        
        recommendations = "Recommendations:\n" ++
                         "---------------\n" ++
                         generateRecommendations benchResults
    
    in header ++ summary ++ details ++ recommendations
  where
    generateSummaryStats results =
        let totalTests = length results
            avgTime = sum [time | (_, time, _) <- results] / fromIntegral totalTests
            avgAccuracy = sum [acc | (_, _, acc) <- results] / fromIntegral totalTests
        in printf "Total tests run: %d\n" totalTests ++
           printf "Average execution time: %.2f ms\n" avgTime ++
           printf "Average accuracy: %.2e\n" avgAccuracy

-- | Helper Functions

-- | Matrix multiplication with specific block size
multiplyWithBlockSize :: Int -> (M.Matrix, M.Matrix) -> M.Matrix
multiplyWithBlockSize blockSize (m1, m2) = 
    let (r1, c1) = M.dimensions m1
        (r2, c2) = M.dimensions m2
        result = M.zeros r1 c2
        
        -- Multiply blocks
        blockMultiply i j k =
            let block1 = M.submatrix m1 i k blockSize blockSize
                block2 = M.submatrix m2 k j blockSize blockSize
                prod = M.multiply block1 block2
            in case prod of
                Just p -> M.addInPlace result i j p
                Nothing -> error "Block multiplication failed"
        
        -- Generate block indices
        indices = [(i, j, k) | i <- [0,blockSize..r1-1],
                              j <- [0,blockSize..c2-1],
                              k <- [0,blockSize..c1-1]]
    
    in foldr (\idx acc -> blockMultiply (fst3 idx) (snd3 idx) (thd3 idx) acc) 
             result indices

-- | Matrix inversion with specified precision
inverseWithPrecision :: Double -> M.Matrix -> M.Matrix
inverseWithPrecision prec m = 
    let n = M.rows m
        -- Use iterative refinement for specified precision
        refine x r count
            | count >= maxIter = x
            | M.norm r < prec = x
            | otherwise = 
                case LA.solve m r of
                    Just dx -> 
                        let x' = M.add x dx
                            r' = M.subtract (M.multiply m x') (M.identity n)
                        in refine x' r' (count + 1)
                    Nothing -> x
    in case LA.solve m (M.identity n) of
        Just x0 -> 
            let r0 = M.subtract (M.multiply m x0) (M.identity n)
            in refine x0 r0 0
        Nothing -> error "Initial solve failed"
  where
    maxIter = 100

-- | Eigenvalue computation with specified iteration count
eigenvaluesWithIterations :: Int -> M.Matrix -> V.Vector
eigenvaluesWithIterations maxIter m = 
    let -- Use QR iteration with specified max iterations
        iterate m' count
            | count >= maxIter = m'
            | isConverged m' = m'
            | otherwise = 
                case LA.qrDecomposition m' of
                    Just (q, r) -> 
                        let m'' = M.multiply r q
                        in iterate m'' (count + 1)
                    Nothing -> m'
        
        -- Extract diagonal elements as eigenvalues
        final = iterate m 0
        diag = [M.get final i i | i <- [0..M.rows m - 1]]
    
    in V.fromList diag
  where
    isConverged m' = 
        let offDiagonal = sum [abs (M.get m' i j) | i <- [0..M.rows m - 1],
                                                   j <- [0..M.cols m - 1],
                                                   i /= j]
        in offDiagonal < 1e-10

-- | Generate normal random number using Box-Muller transform
generateNormal :: Double -> Double -> Double -> Double
generateNormal mu sigma seed =
    let -- Use seed to generate two uniform random numbers
        u1 = abs (sin (seed * 127.1)) -- Pseudo-random between 0 and 1
        u2 = abs (sin (seed * 311.7))
        -- Box-Muller transform
        r = sqrt (-2 * log u1)
        theta = 2 * pi * u2
        z = r * cos theta
    in mu + sigma * z

-- | Helper function to chunk list into sublists
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- | Tuple accessors
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

thd3 :: (a, b, c) -> c
thd3 (_, _, z) = z