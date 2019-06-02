{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Expression (dna2rna) where

import Data.Colour.RGBSpace.HSV
import Data.Colour.SRGB
import Data.Word
import Data.List

import System.Random.Shuffle
import Control.Monad.Random.Strict hiding (interleave)
import Data.Hashable

import DNA
import RNA

dna2rna :: DNA -> RNA
dna2rna [] = dna2rna [0]
dna2rna [x] = dna2rna [x,x]
dna2rna dna@(col1:col2:ops) = fst $ go ops colors
  where
    seed = hash dna
    colors = cycle $ foldl1 interleave $ map (derivedColors seed) $ map baseColor $ nub [col1, col2]

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
            ]
        unaryOps = unary <$>
            [ Rotate  (0  … 2*pi)
            , Invert
            , Swirl   (-1 … 1)
            , Dilated (from [2,3,4,8])
            ]

        op = x `div` 16
        arg = x `mod` 16

        (…) :: Double -> Double -> Double
        l … u = l + a01 * (u - l)

        (……) :: Int -> Int -> Int
        l …… u = min u $ floor $ fromIntegral l … (fromIntegral u+1)

        from = indexMod arg

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

shufDet :: Int -> [b] -> [b]
shufDet seed xs = evalRand (shuffleM xs) $ mkStdGen seed

derivedColors :: Int -> RGB Double -> [RGB Double]
derivedColors seed (hsvView -> (h,s,v)) =
    hsv h s v :
    shufDet seed
        [ hsv (h+20) s v
        , hsv (h-20) s v
        , hsv h ((s+1)/2) v
        , hsv h (s/2) v
        , hsv h s ((v+1)/2)
        , hsv h s (v/2)
        ]

baseColor :: Word8 -> RGB Double
-- baseColor = _baseColorRelativeTo (hsv 15 1 1)
-- baseColor = baseColorRelativeTo (julianeColors !! 5)
baseColor n = indexMod n julianeColors

julianeColors :: [RGB Double]
julianeColors = map (toSRGB.sRGB24read)
    [ "#ba0100"
    , "#f7c616"
    , "#141995"
    , "#2183cc"
    , "#076b1d"
    , "#f4a40d"
    ]

_baseColorRelativeTo :: RGB Double -> Word8 -> RGB Double
_baseColorRelativeTo (hsvView -> (h,s,v)) n =
    hsv (h + tau) s v
  where
    tau = (fromIntegral n / fromIntegral (maxBound `asTypeOf` n)) * 360

indexMod :: Word8 -> [a] -> a
indexMod n xs = xs !! n'
  where
    n' = fromIntegral n `mod` length xs
