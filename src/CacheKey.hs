{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module CacheKey where

import Data.Typeable

data CacheKey where
    CacheKey :: forall a. (Ord a, Typeable a) => a -> CacheKey

instance Eq CacheKey where
    CacheKey (x::a) == CacheKey (y::b) = case eqT @a @b of
        Just Refl -> x == y
        Nothing -> False

instance Ord CacheKey where
    compare (CacheKey (x::a)) (CacheKey (y::b)) = case eqT @a @b of
        Just Refl -> x `compare` y
        Nothing -> typeOf x `compare` typeOf y
