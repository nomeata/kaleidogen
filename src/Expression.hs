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
dna2rna = fromSDNA . fromDNA

fromSDNA :: StructuredDNA -> RNA
fromSDNA (SDNA col1 col2 shape) = fst $ go shape colors
  where
    colors = cycle $ foldl1 interleave $ map derivedColors $ map baseColor $ nub [col1, col2]

    go _ [] = error "finite list in fromSDNA"
    go N (c:cols) = (, cols) $ Solid c
    go (B op arg t1 t2) cols = (, cols2) $
        indexMod op
            -- [ Blend a01           r1 r2
            [ Checker (1  … 6)    r1 r2
            , Rotate  (0  … 2*pi) (Gradient r1 r2)
            , Invert              (Gradient r1 r2)
            , Swirl   (-1 … 1)    (Gradient r1 r2)
            , Rays    (2  …… 8)   r1 r2
            , Gradient            r1 r2
            , Ontop   (0.5 … 0.9) r1 r2
            ]
      where
        (…) :: Double -> Double -> Double
        l … u = l + a01 * (u - l)

        (……) :: Int -> Int -> Int
        l …… u = min u $ floor $ fromIntegral l … (fromIntegral u+1)

        a01 :: Double
        a01 = fromIntegral arg / fromIntegral (maxBound `asTypeOf` arg)

        (r1, cols1) = go t1 cols
        (r2, cols2) = go t2 cols1

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave (x:xs) ys = x : interleave ys xs

derivedColors :: RGB Double -> [RGB Double]
derivedColors (hsvView -> (h,s,v)) =
    hsv h s v :
    -- hsv h ((s+1)/2) v : hsv h (s/2) v :
    -- hsv h s ((v+1)/2) : hsv h s (v/2) :
    hsv (h+30) s v : hsv (h-30) s v :
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
