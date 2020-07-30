module Test.NoPeeking.Solutions where

import Prelude
import Global (readFloat)
import Math (e, pi, sqrt)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

circleArea :: Number -> Number
circleArea r = pi * r * r

addE :: String -> Number
addE s = readFloat s + e
