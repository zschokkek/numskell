# Numskell: A Numerical Computing Library in Haskell

Pure Haskell implementation of numerical computing algorithms with a focus on performance and type safety.

## Features
- Matrix and Vector operations with strong type guarantees
- Statistical analysis and hypothesis testing
- Robust CSV data import with type inference
- Linear algebra algorithms optimized for numerical stability
- Comprehensive testing suite and benchmarking tools

## Installation

Ensure you have GHC and Cabal installed. Then:

```bash
git clone https://github.com/yourusername/numskell.git
cd numskell
cabal update
cabal install --only-dependencies
```

## Dependencies
```yaml
dependencies:
  - base >= 4.14 && < 5
  - vector
  - containers
  - time
  - QuickCheck
  - HUnit
  - deepseq
```

## Usage Examples

### Basic Matrix Operations
```haskell
import qualified Numeric.Matrix as M
import qualified Numeric.Vector as V

-- Create a matrix
let m = M.fromLists [[1,2,3], [4,5,6]]

-- Matrix multiplication
case M.multiply m (M.transpose m) of
    Just result -> -- Handle result
    Nothing     -> -- Handle error
```

### Statistical Analysis
```haskell
import qualified Statistics as S

-- Fit a linear model
let (x, y) = generateTestData
case S.fitLinearModel x y of
    Just model -> print $ S.coefficients model
    Nothing    -> putStrLn "Failed to fit model"
```

### Data Import
```haskell
import qualified DataImport as DI

-- Read CSV with default options
result <- DI.readCSV "data.csv"
case result of
    Right tabularData -> -- Process data
    Left err -> putStrLn $ "Error: " ++ err
```

## Testing

Run the test suite:

```bash
cabal test
```

Or run specific tests:

```haskell
import Test

main = do
    -- Run QuickCheck tests
    quickCheck prop_vector_addition_commutative
    -- Run HUnit tests
    runTestTT test_numerical_stability
```

## Building

To build the library:

```bash
cabal build
```

For optimization, use:

```bash
cabal build -O2
```
