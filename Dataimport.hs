{-|
Module      : DataImport
Description : CSV and data import functionality
Copyright   : (c) Kyle Zschoke, 2025
License     : BSD3

This module provides robust data import capabilities, handling CSV files
while preserving metadata and providing convenient conversions to the 
mathematical types established in /Numeric. It includes type inference, missing value handling,
and efficient large file processing.
-}

module DataImport 
    ( -- * Types
      TabularData(..)
    , DataType(..)
    , DataValue(..)
    , ImportOptions(..)
      -- * Core Import Functions
    , readCSV
    , readCSVWithOptions
    , writeCSV
      -- * Data Access
    , getColumn
    , getRow
    , selectColumns
    , filterRows
      -- * Type Conversion
    , toMatrix
    , fromMatrix
    , toVector
      -- * Data Manipulation
    , dropNA
    , fillNA
    , mutate
    , summarize
    ) where

import qualified Numeric.Matrix as M
import qualified Numeric.Vector as V
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Time (UTCTime, parseTimeM, defaultTimeLocale)
import System.IO (Handle, IOMode(..), openFile, hClose)
import Control.Exception (bracket)
import Data.Char (isNumber)
import Data.List (transpose, nub)
import Text.Printf (printf)

-- | Core data types for representing tabular data
data TabularData = TabularData {
    dataMatrix :: [[DataValue]],    -- Raw data values
    rowNames :: Maybe [String],     -- Optional row names
    columnNames :: [String],        -- Column names
    columnTypes :: [DataType],      -- Inferred types for each column
    metadata :: Map.Map String String  -- Additional metadata
} deriving Show

-- | Possible data types in our tabular data
data DataType = 
    NumericType        -- ^ Floating point numbers
    | IntegerType      -- ^ Whole numbers
    | CategoricalType  -- ^ Factor variables
    | TemporalType     -- ^ Date/time data
    | TextType         -- ^ String data
    | BooleanType      -- ^ True/False values
    deriving (Show, Eq)

-- | Individual data values with type safety
data DataValue = 
    NumericVal Double
    | IntegerVal Integer
    | CategoricalVal String
    | TemporalVal UTCTime
    | TextVal String
    | BooleanVal Bool
    | NA
    deriving (Show, Eq)

-- | Options for customizing CSV import behavior
data ImportOptions = ImportOptions {
    delimiter :: Char,              -- ^ CSV delimiter character
    hasHeader :: Bool,             -- ^ Whether file has header row
    naStrings :: [String],         -- ^ Strings to interpret as NA
    dateFormat :: String,          -- ^ Format for parsing dates
    chunkSize :: Int,              -- ^ Size of chunks for large files
    typeInference :: Bool          -- ^ Whether to perform type inference
} deriving Show

-- | Default import options
defaultOptions :: ImportOptions
defaultOptions = ImportOptions {
    delimiter = ',',
    hasHeader = True,
    naStrings = ["", "NA", "N/A", "null", "NULL"],
    dateFormat = "%Y-%m-%d",
    chunkSize = 10000,
    typeInference = True
}

-- | Read a CSV file with default options
readCSV :: FilePath -> IO (Either String TabularData)
readCSV = readCSVWithOptions defaultOptions

-- | Read a CSV file with custom options
readCSVWithOptions :: ImportOptions -> FilePath -> IO (Either String TabularData)
readCSVWithOptions opts filepath = bracket 
    (openFile filepath ReadMode)
    hClose
    (\handle -> do
        contents <- hGetContents handle
        let rows = parseCSV (delimiter opts) contents
        case rows of
            [] -> return $ Left "Empty CSV file"
            (header:rest) | hasHeader opts -> do
                let colNames = if hasHeader opts then header else defaultColumnNames (length header)
                types <- if typeInference opts
                         then inferTypes (transpose rest)
                         else return (replicate (length header) TextType)
                values <- parseValues types rest
                return $ Right TabularData {
                    dataMatrix = values,
                    rowNames = Nothing,
                    columnNames = colNames,
                    columnTypes = types,
                    metadata = Map.fromList [
                        ("source", filepath),
                        ("date_imported", show currentTime),
                        ("num_rows", show $ length rest),
                        ("num_cols", show $ length header)
                    ]
                }
            _ -> return $ Left "Invalid CSV format")

-- | Parse CSV content into rows
parseCSV :: Char -> String -> [[String]]
parseCSV delim content = 
    let rows = lines content
        parseRow = splitOn delim
    in map parseRow rows

-- | Infer types for each column
inferTypes :: [[String]] -> IO [DataType]
inferTypes columns = return $ map inferColumnType columns
  where
    inferColumnType :: [String] -> DataType
    inferColumnType values = 
        let nonNA = filter (not . isNA) values
            uniqueCount = length $ nub nonNA
            totalCount = length nonNA
            proportionUnique = fromIntegral uniqueCount / fromIntegral totalCount
        in if all isNumeric nonNA then
               if all isInteger nonNA then IntegerType else NumericType
           else if all isTemporalString nonNA then TemporalType
           else if all isBooleanString nonNA then BooleanType
           else if proportionUnique < 0.2 then CategoricalType
           else TextType

-- | Parse string values into strongly typed DataValues
parseValues :: [DataType] -> [[String]] -> IO [[DataValue]]
parseValues types rows = 
    mapM (zipWithM parseValue types) rows
  where
    parseValue :: DataType -> String -> IO DataValue
    parseValue dtype str | isNA str = return NA
    parseValue NumericType str = 
        case reads str of
            [(n, "")] -> return $ NumericVal n
            _ -> return NA
    parseValue IntegerType str =
        case reads str of
            [(n, "")] -> return $ IntegerVal n
            _ -> return NA
    parseValue TemporalType str =
        case parseTimeM True defaultTimeLocale (dateFormat defaultOptions) str of
            Just t -> return $ TemporalVal t
            Nothing -> return NA
    parseValue BooleanType str =
        case map toLower str of
            "true" -> return $ BooleanVal True
            "false" -> return $ BooleanVal False
            _ -> return NA
    parseValue _ str = return $ TextVal str

-- | Convert TabularData to Matrix for numerical computations
toMatrix :: TabularData -> Either String M.Matrix
toMatrix td = do
    -- Extract numeric columns only
    let numericCols = [(i, col) | (i, (col, typ)) <- zip [0..] (zip (transpose $ dataMatrix td) (columnTypes td)),
                                  typ `elem` [NumericType, IntegerType]]
    if null numericCols
        then Left "No numeric columns found"
        else Right $ M.fromLists [[extractNumeric v | v <- col] | (_, col) <- numericCols]
  where
    extractNumeric (NumericVal x) = x
    extractNumeric (IntegerVal x) = fromIntegral x
    extractNumeric _ = 0.0  -- Replace NA with 0.0 (could be configurable)

-- | Convert Matrix back to TabularData
fromMatrix :: M.Matrix -> [String] -> TabularData
fromMatrix mat colNames = TabularData {
    dataMatrix = [[NumericVal x | x <- row] | row <- M.toLists mat],
    rowNames = Nothing,
    columnNames = colNames,
    columnTypes = replicate (M.cols mat) NumericType,
    metadata = Map.empty
}

-- | Select specific columns by name
selectColumns :: [String] -> TabularData -> Either String TabularData
selectColumns names td = do
    let colIndices = mapM (`elemIndex` columnNames td) names
    case colIndices of
        Nothing -> Left "Column name not found"
        Just indices -> Right td {
            dataMatrix = transpose $ map (transpose (dataMatrix td) !!) indices,
            columnNames = names,
            columnTypes = map (columnTypes td !!) indices
        }

-- | Filter rows based on a predicate
filterRows :: ([DataValue] -> Bool) -> TabularData -> TabularData
filterRows pred td = td {
    dataMatrix = filter pred (dataMatrix td),
    rowNames = fmap (map snd . filter (pred . fst) . zip (dataMatrix td)) (rowNames td)
}

-- | Remove rows with NA values
dropNA :: TabularData -> TabularData
dropNA = filterRows (not . any isNA)

-- | Fill NA values using a strategy
fillNA :: DataValue -> TabularData -> TabularData
fillNA replacement td = td {
    dataMatrix = map (map replaceNA) (dataMatrix td)
}
  where
    replaceNA NA = replacement
    replaceNA x = x

-- | Helper functions
isNA :: String -> Bool
isNA s = s `elem` naStrings defaultOptions

isNumeric :: String -> Bool
isNumeric s = case reads s :: [(Double, String)] of
    [(_, "")] -> True
    _ -> False

isInteger :: String -> Bool
isInteger s = case reads s :: [(Integer, String)] of
    [(_, "")] -> True
    _ -> False

isTemporalString :: String -> Bool
isTemporalString s = case parseTimeM True defaultTimeLocale (dateFormat defaultOptions) s of
    Just _ -> True
    Nothing -> False

isBooleanString :: String -> Bool
isBooleanString s = map toLower s `elem` ["true", "false"]

defaultColumnNames :: Int -> [String]
defaultColumnNames n = map (\i -> "V" ++ show i) [1..n]

splitOn :: Char -> String -> [String]
splitOn delim = foldr f [[]] 
  where
    f c (x:xs) | c == delim = []:x:xs
               | otherwise = (c:x):xs