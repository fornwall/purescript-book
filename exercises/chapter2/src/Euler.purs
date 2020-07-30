module Euler where

import Prelude
import Data.List (List, range, filter)
import Data.Foldable (sum)

ns :: Int -> List Int
ns n = range 0 (n - 1)

multiples :: Int -> List Int
multiples n = filter (\element -> mod element 3 == 0 || mod element 5 == 0) (ns n)

answer :: Int -> Int
answer n = sum (multiples n)
