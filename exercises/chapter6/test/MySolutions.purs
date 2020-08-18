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

data NonEmpty a = NonEmpty a (Array a)

instance eqNonEmpty :: Eq a => Eq (NonEmpty a) where
  eq (NonEmpty e1 a1) (NonEmpty e2 a2) = e1 == e2 && a1 == a2

instance showNonEmpty :: Show a => Show (NonEmpty a) where
  show (NonEmpty e1 a1) = show e1 <> ", " <> show a1

--class Semigroup a where
--  append :: a -> a -> a
instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty e1 a1) (NonEmpty e2 a2) = NonEmpty e1 (a1 <> [ e2 ] <> a2)

--class Functor f where
--  map :: forall a b. (a -> b) -> f a -> f b
instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty e a) = NonEmpty (f e) (map f a)
