{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Expression (dna2rna) where

import Data.Colour.RGBSpace.HSV
import Data.Word
import Data.List

import DNA
import RNA

dna2rna :: DNA -> RNA
dna2rna [] = dna2rna [0]
dna2rna [x] = dna2rna [x,x]
dna2rna (col1:col2:ops) = fst $ go ops colors
  where
    colors = cycle $ foldl1 interleave $ map derivedColors $ map baseColor $ nub [col1, col2]

    go _ [] = error "finite list in fromSDNA"
    go []  (c:cols) = (, cols) $ Solid c
    go (x:rest) cols
        | null rest = indexMod op binaryOps
        | otherwise = indexMod op (binaryOps ++ unaryOps)
      where
        binaryOps = binary <$>
            [ Checker (1  … 6)
            , Rays    (2  …… 8)
            , Gradient
            , Ontop   (0.5 … 0.9)
            -- , Blend a01
            ]
        unaryOps = unary <$>
            [ Rotate  (0  … 2*pi)
            , Invert
            , Swirl   (-1 … 1)
            ]

        op = x `div` 16
        arg = x `mod` 16


        (…) :: Double -> Double -> Double
        l … u = l + a01 * (u - l)

        (……) :: Int -> Int -> Int
        l …… u = min u $ floor $ fromIntegral l … (fromIntegral u+1)

        a01 :: Double
        a01 = fromIntegral arg / 16

        binary f = (f r1 r2, cols2)
          where
            (rest1, rest2) = splitInHalf rest
            (r1, cols1) = go rest1 cols
            (r2, cols2) = go rest2 cols1

        unary f = (f r1, cols1)
          where
            (r1, cols1) = go rest cols

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave (x:xs) ys = x : interleave ys xs

splitInHalf :: [a] -> ([a],[a])
splitInHalf xs = splitAt n xs
  where n = length xs `div` 2

derivedColors :: RGB Double -> [RGB Double]
derivedColors (hsvView -> (h,s,v)) =
    hsv h s v :
    hsv (h+20) s v : hsv (h-20) s v :
    hsv h ((s+1)/2) v : hsv h (s/2) v :
    hsv h s ((v+1)/2) : hsv h s (v/2) :
    []

baseColor :: Word8 -> RGB Double
-- baseColor 0 = hsv 0   1 0
-- baseColor 1 = hsv 0   0 1
baseColor n = hsv tau 0.7 1
  where
    tau = (fromIntegral (n-2) / fromIntegral (maxBound `asTypeOf` n - 2)) * 360

indexMod :: Word8 -> [a] -> a
indexMod n xs = xs !! n'
  where
    n' = fromIntegral (n-1) `mod` length xs
