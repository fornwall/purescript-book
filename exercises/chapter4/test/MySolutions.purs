module Test.MySolutions where

import Prelude
import Data.Array (filter, length, (..))
import Control.MonadZero (guard)
import Data.Int (quot, rem)
import Data.Array (cons)
import Data.Foldable (foldl, foldr)

isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven number = isEven $ number - 2

countEven :: Array Int -> Int
countEven = length <<< filter isEven

squared :: Array Number -> Array Number
squared = map (\n -> n*n)

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter (_ >= 0.0)

infix 8 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite array = (_ >= 0.0) <$?> array

_factors :: Int -> Array (Array Int)
_factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

isPrime :: Int -> Boolean
isPrime 1 = false
isPrime number = length (_factors number) == 1

cartesianProduct :: forall a. (Array a) -> (Array a) -> (Array (Array a))
cartesianProduct array1 array2 = do
  i <- array1
  j <- array2
  pure [i, j]

triples :: Int -> Array (Array Int)
triples n = do
  i <- 1 .. n
  j <- i .. n
  k <- j .. n
  guard $ i*i + j*j == k*k
  pure [i, j, k]

factorizations :: Int -> Array Int
factorizations n = factorizations' 2 n []
  where
  factorizations' :: Int -> Int -> Array Int -> Array Int
  factorizations' _ 1 result = result

  factorizations' divisor dividend result =
    if (dividend `mod` divisor) == 0 then
      factorizations' divisor (quot dividend divisor) (cons divisor result)
    else
      factorizations' (divisor + 1) dividend result

allTrue :: Array Boolean -> Boolean
allTrue array = foldl (\accumulator element -> accumulator && element) true array

fibTailRec :: Int -> Int
fibTailRec n = fib' n 0 0 1
  where
  fib' :: Int -> Int -> Int -> Int -> Int
  fib' limit count n1 n2 =
    if limit == count then
      n1 + n2
    else
      fib' limit (count + 1) (n1 + n2) n1

reverse :: Array Int -> Array Int
reverse = foldl (\acc el -> [el] <> acc) []