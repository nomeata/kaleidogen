{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module DNA where

import Control.Monad.Random.Strict
import Data.Hashable
import Data.List
import Data.Word
import Data.Tuple
import Data.Ord

type DNA = [Word8]

data StructuredDNA = SDNA
    { color1 :: Word8
    , color2 :: Word8
    , shape :: ShapeTree
    }

data ShapeTree = B Word8 Word8 ShapeTree ShapeTree | N
  deriving (Ord, Eq)


fromDNA :: DNA -> StructuredDNA
fromDNA (color1:color2:rest) = SDNA color1 color2 (fst (go rest))
  where
    go :: DNA -> (ShapeTree, DNA)
    go (0:rest)   = (N, rest)
    go (n:a:rest) = (B n a t1 t2, rest2)
      where (t1, rest1) = go rest
            (t2, rest2) = go rest1
    go _          = (N, [])

fromDNA [color1] = fromDNA [color1, color1]
fromDNA [] = fromDNA [0]

toDNA :: StructuredDNA -> DNA
toDNA (SDNA color1 color2 shape)
    = color1 : color2 : go shape
  where
    go (B n a t1 t2) = n : a : go t1 ++ go t2
    go N = [0]

crossover :: Int -> DNA -> DNA -> DNA
crossover seed x' y' =
    toDNA $ evalRand (crossover' (fromDNA x) (fromDNA y)) $ mkStdGen (hash (seed,x,y))
  where
    [x, y] = sortBy (comparing hash) [x', y']

crossover' :: MonadRandom m => StructuredDNA -> StructuredDNA -> m StructuredDNA
crossover' = start
  where
    start (SDNA col11 col12 shape1) (SDNA col21 col22 shape2)
         = SDNA <$> oneOf col11 col21 <*> oneOf col12 col22 <*> root shape1 shape2

    root N N = B <$> getRandom <*> getRandom <*> pure N <*> pure N
    root t1 t2 = go t1 t2

    go t1 t2 = w $
        [ 1 =: pure N
        | N <- pure t1, N <- pure t2
        ] ++
        [ 1 =: B <$> getRandom <*> getRandom <*> pure t1 <*> pure t2
        ] ++
        [ 1 =: go t1a t1b >>= \t1' -> go t1' t2
        | B _ _ t1a t1b <- pure t1
        ] ++
        [ 1 =: go t2a t2b >>= \t2' -> go t1 t2'
        | B _ _ t2a t2b <- pure t2
        ] ++
        [ 4 =: B <$> mixOp n1 n2 <*> mixArg a1 a2 <*> go t1a t2a <*> go t1b t2b
        | B n1 a1 t1a t1b <- pure t1, B n2 a2 t2a t2b <- pure t2
        ] ++
        [ 1 =: B n1 a1 <$> go t1a t2 <*> pure t1b
        | B n1 a1 t1a t1b <- pure t1
        ] ++
        [ 1 =: B n1 a1 <$> pure t1a <*> go t1b t2
        | B n1 a1 t1a t1b <- pure t1
        ] ++
        [ 1 =: B n2 a2 <$> go t1 t2a <*> pure t2b
        | B n2 a2 t2a t2b <- pure t2
        ] ++
        [ 1 =: B n2 a2 <$> pure t2a <*> go t1 t2b
        | B n2 a2 t2a t2b <- pure t2
        ]

    (=:) = (,)
    infixr 0 =:
    w = join . weighted . map swap

    oneOf a b = w [ 1 =: pure a, 1 =: pure b]

    mixArg n1 n2 = w
        [ 1 =: pure $ (n1 `div` 2 + n2 `div` 2)
        , 1 =: pure $ (n1 + n2)
        ]
    mixOp n1 n2 = w
        [ 2 =: pure $ n1
        , 2 =: pure $ n2
        , 1 =: pure $ n1 + n2
        ]

blankDNA :: DNA
blankDNA = [0]

initialDNAs :: [DNA]
initialDNAs = [ [x,x] | x <- [0,43..255] ]
