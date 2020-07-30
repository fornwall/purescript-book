module Test.MySolutions where

import Prelude
import Data.Array (filter, length, (..))
import Control.MonadZero (guard)

isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven number = isEven $ number - 2

countEven :: Array Int -> Int
-- countEven arr = length (filter (\n -> (isEven n)) arr)
-- countEven arr = (filter isEven >>> length) arr
-- countEven arr = length $ filter isEven $ arr
-- countEven arr = length $ isEven `filter` arr
countEven = filter isEven >>> length

squared :: Array Number -> Array Number
squared array = map (\n -> n*n) array

keepNonNegative :: Array Number -> Array Number
keepNonNegative array = filter (\n -> n >= 0.0) array

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
