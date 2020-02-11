# hafar

Hafar is an implementation of affine arithmetic in haskell.

## Prerequisites

 - stack >2.1.3

## Building

To build the library, simply run `stack build`.

## Example

All operations with affine forms must be done inside the AFM monad.

```
import Numeric.Interval hiding (interval)

x1 = do
  a <- newFromInterval $ 4...6
  b <- newFromInterval $ 4...6
  return . interval $ a - b

evalAFM x1 -- evaluates to approximately -2 ... 2

x2 = do
  a <- newFromInterval $ 4...6
  return . interval $ a - a

evalAFM x2 -- evaluates to approximately 0 ... 0

```
