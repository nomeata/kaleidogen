module SBNL where

import Data.List

type SBNL a = [a]

empty :: SBNL a
empty = []

member :: Eq a => a -> SBNL a -> Bool
member = elem

flipMember :: Eq a => Int -> SBNL a -> a -> SBNL a
flipMember n s x | member x s    = delete x s
                 | length s >= n = x : drop 1 s
                 | otherwise     = x : s
            --   | otherwise  = take n $ x : s

toList :: SBNL a -> [a]
toList = id

size :: SBNL a -> Int
size = length
