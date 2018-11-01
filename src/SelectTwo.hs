{-# LANGUAGE DeriveFunctor #-}
{- |
 A data structure that selects at most two elements, changing the most recently added.
-}
module SelectTwo where

data SelectTwo a
    = NoneSelected
    | OneSelected a
    | TwoSelected a a
  deriving Functor

empty :: SelectTwo a
empty = NoneSelected

singleton :: a -> SelectTwo a
singleton = OneSelected

flip :: Eq a => SelectTwo a -> a -> SelectTwo a
flip NoneSelected      x             = OneSelected x
flip (OneSelected y)   x | x == y    = NoneSelected
                         | otherwise = TwoSelected y x
flip (TwoSelected y z) x | x == z    = OneSelected y
                         | x == y    = OneSelected z
                         | otherwise = TwoSelected y x

member :: Eq a => a -> SelectTwo a -> Bool
member _ NoneSelected = False
member x (OneSelected y) = x == y
member x (TwoSelected y z) = x == y || x == z
