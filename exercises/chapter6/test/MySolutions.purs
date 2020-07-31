module Test.MySolutions where

import Prelude

newtype Complex = Complex
  { real :: Number
  , imaginary :: Number
  }

{--
"show is defined by a type class in the Prelude module called Show, which is defined as follows:
  class Show a where
    show :: a -> String
This code declares a new type class called Show, which is parameterized by the type variable a.
A type class instance contains implementations of the functions defined in a type class, specialized to a particular type."
--}
instance showComplex :: Show Complex where
  show (Complex c) | c.imaginary >= 0.0 = (show c.real) <> "+" <> (show c.imaginary) <> "i"
                   | otherwise = (show c.real) <> (show c.imaginary) <> "i"

instance eqComplex :: Eq Complex where
  eq (Complex a) (Complex b) = a.real == b.real && a.imaginary == b.imaginary
