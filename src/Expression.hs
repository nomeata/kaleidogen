module Expression (dna2rna) where

import Data.Colour
import Data.Colour.Names
import Data.Word

import DNA
import RNA

dna2rna :: DNA -> RNA
dna2rna = go . fromDNA

go :: TNA -> RNA
go (N arg) = Solid (baseColor arg)
go (B op arg t1 t2) = indexMod op
    [ Solid (baseColor arg)
    , Blend a01           r1 r2
    , Checker (1  … 6)    r1 r2
    , Rotate  (0  … 2*pi) r1
    , Invert              r1
    , Swirl   (-1 … 1)    r1
    , Rays    (1  …… 8)   r1 r2
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

    r1 = go t1
    r2 = go t2

baseColor :: Word8 -> Colour Double
baseColor n = indexMod n colors
  where
    colors = [ black, white
             , red, green, blue
             , cyan, magenta, yellow
             ]

indexMod :: Word8 -> [a] -> a
indexMod n xs = xs !! n'
  where
    n' = fromIntegral (n-1) `mod` length xs
