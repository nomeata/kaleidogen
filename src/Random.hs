{-# LANGUAGE CPP #-}
module Random () where

#if MIN_VERSION_random(1,2,0)

#else

-- Until reflex-platform has random-1.2

import System.Random
import qualified System.Random.SplitMix as SM

instance RandomGen SM.SMGen where
  next = SM.nextInt
  {-# INLINE next #-}
  -- genWord32 = SM.nextWord32
  -- {-# INLINE genWord32 #-}
  -- genWord64 = SM.nextWord64
  -- {-# INLINE genWord64 #-}
  split = SM.splitSMGen
  {-# INLINE split #-}

#endif
