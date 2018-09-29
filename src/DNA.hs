{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}

module DNA where

import Control.Monad.Random.Strict
import Data.Hashable
import Data.List
import Data.Word
import Data.Tuple
import GHC.Generics

type DNA = [Word8]

data TNA = B Word8 Word8 TNA TNA | N Word8
  deriving (Ord, Eq, Generic, Hashable)


fromDNA :: DNA -> TNA
fromDNA = fst . go
  where
    go :: DNA -> (TNA, DNA)
    go (0:a:rest) = (N a, rest)
    go (n:a:rest) = (B n a t1 t2, rest2)
      where (t1, rest1) = go rest
            (t2, rest2) = go rest1
    go _          = (N 0, [])

toDNA :: TNA -> DNA
toDNA (B n a t1 t2) = n : a : toDNA t1 ++ toDNA t2
toDNA (N a) = [0, a]

crossover :: Int -> DNA -> DNA -> DNA
crossover seed x' y' =
    toDNA $ evalRand (crossover' x y) $ mkStdGen (hash (seed,x,y))
  where
    [x, y] = sort [fromDNA x', fromDNA y']

crossover' :: MonadRandom m => TNA -> TNA -> m TNA
crossover' = start
  where
    start (N a1) (N a2) = B <$> getRandom <*> getRandom <*> pure (N a1) <*> pure (N a2)
    start t1 t2 = go t1 t2

    go t1 t2 = w $
        [ 1 =: N <$> mixArg a1 a2
        | N a1 <- pure t1, N a2 <- pure t2
        ] ++
        [ 1 =: B n1 <$> mixArg a1 a2 <*> pure t1a <*> pure t1b
        | B n1 a1 t1a t1b <- pure t1, N a2 <- pure t2
        ] ++
        [ 1 =: B n2 <$> mixArg a1 a2 <*> pure t2a <*> pure t2b
        | N a1 <- pure t1, B n2 a2 t2a t2b <- pure t2
        ] ++
        [ 1 =: B <$> getRandom <*> getRandom <*> pure t1 <*> pure t2
        ] ++
        [ 1 =: go t1a t1b >>= \t1' -> go t1' t2
        | B _ _ t1a t1b <- pure t1
        ] ++
        [ 1 =: go t2a t2b >>= \t2' -> go t1 t2'
        | B _ _ t2a t2b <- pure t2
        ] ++
        [ 4 =: B <$> mixOp n1 n2 <*> mixArg a1 a2 <*> oneOf t1a t2a <*> oneOf t1b t2b
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
initialDNAs = [ [0,x] | x <- 0:1:[2,34..255] ]
