module Expression (runProgram) where

import Data.Colour
import Data.Colour.Names
import Data.Foldable

import RNA

runProgram :: Program -> RNA
runProgram = collapsStack . foldl' (flip runInst) []

baseColor :: Int -> Colour Double
baseColor n = colors !! n'
  where
    n' = n `mod` length colors
    colors = [ black, white
             , red, green, blue
             , cyan, magenta, yellow
             ]

type Program = [Int]
type Inst = Int

type Stack = [RNA]

runInst :: Inst -> Stack -> Stack
runInst 0 s            = Op0 Gradient : s
runInst 1 (i2:i1:s)    = Op2 Before i1 i2 : s
runInst 2 (i3:i2:i1:s) = Op3 Blur i3 i1 i2  : s
runInst 3 (i2:i1:s)    = Op2 Checker i1 i2  : s
runInst 4 (i:s)        = Op1 Inv i          : s
runInst c (i2:i1:s)    | c >= 10 && c < 17  = Op2 (Rays (c - 9)) i1 i2 : s
runInst c (i:s)        | c >= 20 && c <= 30 = Op1 (Swirl (fromIntegral (c - 25))) i : s
-- fallback
runInst n s  = Op0 (Solid (baseColor n)) : s


collapsStack :: Stack -> RNA
collapsStack [] = Op0 (Solid black)
collapsStack [i] = i
collapsStack (i:is) = Op2 Before fg bg
  where
    bg = i
    fg = collapsStack is
