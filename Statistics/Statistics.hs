{-|
Module      : Statistics
Description : Comprehensive statistical analysis toolkit
Copyright   : (c) Kyle Zschokke, 2025
License     : BSD3
Stability   : experimental

This module provides a comprehensive suite of statistical analysis tools.
It builds upon the linear algebra foundation to implement both basic
and advanced statistical methods, with a focus on numerical stability
and proper handling of edge cases.
-}

module Statistics 
    ( -- * Basic Statistics
      mean
    , weightedMean
    , median
    , mode
    , variance
    , standardDeviation
    , skewness
    , kurtosis
      -- * Regression Analysis
    , LinearModel(..)
    , fitLinearModel
    , predictLinearModel
    , modelSummary
      -- * Hypothesis Testing
    , TestResult(..)
    , tTest
    , chiSquareTest
    , anovaOneWay
      -- * Distribution Analysis
    , Distribution(..)
    , fitDistribution
    , normalPDF
    , normalCDF
    , poissonPMF
    , binomialPMF
      -- * Time Series Analysis
    , TimeSeriesModel(..)
    , fitARIMA
    , forecast
      -- * Multivariate Analysis
    , PCAResult(..)
    , performPCA
    , factorAnalysis
    ) where

import qualified Numeric.LinearAlgebra.Algorithms as LA
import qualified Numeric.Matrix as M
import qualified Numeric.Vector as V
import Data.List (sort, group, maximumBy)
import Data.Ord (comparing)
import Control.Monad (guard)

-- | Type to represent results of statistical tests
data TestResult = TestResult {
    statistic :: Double,        -- ^ Test statistic value
    pValue :: Double,           -- ^ P-value of the test
    degreesOfFreedom :: Int,   -- ^ Degrees of freedom
    confidenceInterval :: Maybe (Double, Double)  -- ^ Optional confidence interval
} deriving (Show, Eq)

-- | Linear regression model representation
data LinearModel = LinearModel {
    coefficients :: V.Vector,          -- ^ Beta coefficients
    standardErrors :: V.Vector,        -- ^ Standard errors of coefficients
    residuals :: V.Vector,            -- ^ Model residuals
    rSquared :: Double,               -- ^ R-squared value
    adjustedRSquared :: Double,       -- ^ Adjusted R-squared
    fStatistic :: Double,             -- ^ F-statistic
    pValueF :: Double,                -- ^ P-value for F-statistic
    vcovMatrix :: M.Matrix            -- ^ Variance-covariance matrix
} deriving Show

-- | Time series model representation
data TimeSeriesModel = TimeSeriesModel {
    arCoefficients :: V.Vector,        -- ^ Autoregressive coefficients
    maCoefficients :: V.Vector,        -- ^ Moving average coefficients
    innovations :: V.Vector,           -- ^ Innovation process
    sigma2 :: Double,                  -- ^ Innovation variance
    logLikelihood :: Double,           -- ^ Model log-likelihood
    aic :: Double,                     -- ^ Akaike Information Criterion
    bic :: Double                      -- ^ Bayesian Information Criterion
} deriving Show

-- | PCA results representation
data PCAResult = PCAResult {
    loadings :: M.Matrix,              -- ^ Principal component loadings
    scores :: M.Matrix,                -- ^ Principal component scores
    eigenvalues :: V.Vector,           -- ^ Eigenvalues
    proportionVar :: V.Vector,         -- ^ Proportion of variance explained
    cumulativeVar :: V.Vector          -- ^ Cumulative proportion of variance
} deriving Show

-- | Distribution type for parametric analysis
data Distribution = 
    Normal Double Double |          -- ^ Normal(μ, σ)
    Poisson Double |               -- ^ Poisson(λ)
    Binomial Int Double |         -- ^ Binomial(n, p)
    Gamma Double Double |         -- ^ Gamma(shape, scale)
    Beta Double Double            -- ^ Beta(α, β)
    deriving Show

-- | Basic Statistical Functions

mean :: V.Vector -> Double
mean xs = V.sum xs / fromIntegral (V.length xs)

weightedMean :: V.Vector -> V.Vector -> Maybe Double
weightedMean xs weights 
    | V.length xs /= V.length weights = Nothing
    | otherwise = Just $ V.sum (V.zipWith (*) xs weights) / V.sum weights

median :: V.Vector -> Double
median xs = 
    let sorted = sort $ V.toList xs
        n = length sorted
    in if even n
       then (sorted !! (n `div` 2 - 1) + sorted !! (n `div` 2)) / 2
       else sorted !! (n `div` 2)

mode :: V.Vector -> V.Vector
mode xs = 
    let grouped = group . sort $ V.toList xs
        maxFreq = maximum $ map length grouped
    in V.fromList . map head $ filter ((== maxFreq) . length) grouped

variance :: V.Vector -> Double
variance xs =
    let m = mean xs
        n = fromIntegral $ V.length xs
    in V.sum (V.map (\x -> (x - m)^2) xs) / (n - 1)

standardDeviation :: V.Vector -> Double
standardDeviation = sqrt . variance

skewness :: V.Vector -> Double
skewness xs =
    let m = mean xs
        s = standardDeviation xs
        n = fromIntegral $ V.length xs
        moment3 = V.sum (V.map (\x -> ((x - m) / s)^3) xs) / n
    in moment3

kurtosis :: V.Vector -> Double
kurtosis xs =
    let m = mean xs
        s = standardDeviation xs
        n = fromIntegral $ V.length xs
        moment4 = V.sum (V.map (\x -> ((x - m) / s)^4) xs) / n
    in moment4 - 3  -- Excess kurtosis (normal = 0)

-- | Regression Analysis

fitLinearModel :: M.Matrix -> V.Vector -> Maybe LinearModel
fitLinearModel x y = do
    -- Add intercept column to X
    let n = M.rows x
        x' = M.augmentWithColumn x (V.replicate n 1)
    
    -- Calculate coefficients using (X'X)^(-1)X'y
    xtx <- M.multiply (M.transpose x') x'
    xtxInv <- LA.inverse xtx
    xty <- M.multiply (M.transpose x') (M.asColumn y)
    beta <- M.multiply xtxInv xty
    
    -- Calculate fitted values and residuals
    yHat <- M.multiply x' beta
    let residuals = V.subtract y (M.toVector yHat)
        
        -- Calculate R-squared
        tss = V.sum $ V.map (^2) $ V.map (subtract (mean y)) y
        rss = V.sum $ V.map (^2) residuals
        rSquared = 1 - rss/tss
        
        -- Calculate adjusted R-squared
        n' = fromIntegral n
        p = fromIntegral $ M.cols x' 
        adjustedRSquared = 1 - (rss/(n'-p))/(tss/(n'-1))
        
        -- Calculate standard errors and F-statistic
        sigma2 = rss/(n'-p)
        vcov = M.scale sigma2 xtxInv
        stdErrs = V.map sqrt $ M.getDiagonal vcov
        fStat = ((tss-rss)/(p-1))/(rss/(n'-p))
        pValueF = 1 - fcdf fStat (p-1) (n'-p)
    
    return LinearModel {
        coefficients = M.toVector beta,
        standardErrors = stdErrs,
        residuals = residuals,
        rSquared = rSquared,
        adjustedRSquared = adjustedRSquared,
        fStatistic = fStat,
        pValueF = pValueF,
        vcovMatrix = vcov
    }

-- | Hypothesis Testing

tTest :: V.Vector -> Double -> Double -> TestResult
tTest sample mu alpha =
    let n = fromIntegral $ V.length sample
        xbar = mean sample
        s = standardDeviation sample
        t = (xbar - mu)/(s/sqrt n)
        df = round (n - 1)
        p = 2 * (1 - studentT df (abs t))
        se = s/sqrt n
        ci = (xbar - criticalValue (1-alpha/2) df * se,
              xbar + criticalValue (1-alpha/2) df * se)
    in TestResult t p df (Just ci)

-- | Distribution Analysis

normalPDF :: Double -> Double -> Double -> Double
normalPDF mu sigma x = 
    (1/(sigma * sqrt (2*pi))) * exp (-(x-mu)^2/(2*sigma^2))

normalCDF :: Double -> Double -> Double -> Double
normalCDF mu sigma x = 
    0.5 * (1 + erf ((x-mu)/(sigma*sqrt 2)))

-- | Time Series Analysis

fitARIMA :: V.Vector -> Int -> Int -> Int -> Maybe TimeSeriesModel
fitARIMA ts p d q = do
    -- Implement ARIMA using Kalman filter and maximum likelihood
    -- This is a complex implementation requiring several steps:
    -- 1. Difference the series d times
    -- 2. Set up state space model
    -- 3. Use Kalman filter for likelihood evaluation
    -- 4. Optimize likelihood using numerical methods
    undefined

-- | Multivariate Analysis

performPCA :: M.Matrix -> Maybe PCAResult
performPCA x = do
    -- Center the data
    let centered = M.centerColumns x
    
    -- Compute covariance matrix
    covMat <- M.multiply (M.transpose centered) centered
    M.scale (1/fromIntegral (M.rows x - 1)) covMat
    
    -- Compute eigendecomposition
    (eigenvals, eigenvecs) <- LA.eigendecomposition covMat
    
    -- Sort by eigenvalues in descending order
    let pairs = zip (V.toList eigenvals) (M.toColumns eigenvecs)
        sorted = reverse $ sort pairs
        (evals, evecs) = unzip sorted
        
        -- Calculate proportion of variance
        totalVar = sum evals
        propVar = map (/totalVar) evals
        cumVar = scanl1 (+) propVar
        
        -- Compute scores
        scores <- M.multiply centered (M.fromColumns evecs)
    
    return PCAResult {
        loadings = M.fromColumns evecs,
        scores = scores,
        eigenvalues = V.fromList evals,
        proportionVar = V.fromList propVar,
        cumulativeVar = V.fromList cumVar
    }

-- | Helper Functions

erf :: Double -> Double
erf x = 2/sqrt pi * integrate 0 x (\t -> exp(-t*t))

integrate :: Double -> Double -> (Double -> Double) -> Double
integrate a b f = 
    let n = 1000  -- Number of intervals
        h = (b-a)/fromIntegral n
        xs = [a + fromIntegral i * h | i <- [0..n]]
        ys = map f xs
    in h * (sum (init ys) + sum (tail ys))/2

studentT :: Int -> Double -> Double
studentT df x = undefined  -- Implement Student's t-distribution

fcdf :: Double -> Double -> Double -> Double
fcdf x df1 df2 = undefined  -- Implement F-distribution CDF

criticalValue :: Double -> Int -> Double
criticalValue p df = undefined  -- Implement inverse Student's t