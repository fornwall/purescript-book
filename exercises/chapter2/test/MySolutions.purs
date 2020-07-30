module Test.MySolutions where

import Global (readFloat)
import Math (e, pi, sqrt)
import Prelude

diagonal :: Number -> Number -> Number
diagonal width height = sqrt (width*width + height*height)

circleArea :: Number -> Number
circleArea radius = radius * radius * pi

addE :: String -> Number
addE string = (readFloat string) + e
