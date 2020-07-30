module Main where

import Effect (Effect)
import Prelude
import Euler (answer)
import Effect.Console (log)

main :: Effect Unit
main = do
  log ("The answer is " <> show (answer 1000))
