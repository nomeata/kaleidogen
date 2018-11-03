{-# LANGUAGE RecordWildCards #-}
module Layout where

import Control.Monad

data Layout a b c = Layout
    { layout :: (Double, Double) -> a -> [(b, (Double, Double), Double)]
    , locate :: (Double, Double) -> a -> (Double,Double) -> Maybe c
    }

mapLayout :: (a -> a') -> Layout a' b c -> Layout a b c
mapLayout f (Layout innerLayout innerLocate) = Layout {..}
  where
    layout (w, h) a = innerLayout (w,h) (f a)
    locate (w,h) a (x,y) = innerLocate (w,h) (f a) (x,y)

layoutMaybe :: Layout a b c -> Layout (Maybe a) b c
layoutMaybe (Layout innerLayout innerLocate) = Layout {..}
  where
    layout _ Nothing = []
    layout (w, h) (Just a) = innerLayout (w,h) a
    locate _ Nothing _ = Nothing
    locate (w,h) (Just a) (x,y) = innerLocate (w,h) a (x,y)

layoutAbove :: Layout a b c -> Layout a' b c' -> Layout (a,a') b (Either c c')
layoutAbove (Layout innerLayout1 innerLocate1) (Layout innerLayout2 innerLocate2) = Layout {..}
  where
    layout (w, h) (a,b) =
        innerLayout1 (w,h/2) a ++ translate 0 (h/2) (innerLayout2 (w,h/2) b)
    locate (w,h) (a,b) (x,y) =
        toEither (innerLocate1 (w,h/2) a (x,y)) (innerLocate2 (w,h/2) b (x,y-h/2))
    toEither (Just x) _ = Just (Left x)
    toEither Nothing (Just x) = Just (Right x)
    toEither Nothing Nothing = Nothing

layoutLarge :: Double -> Layout a a ()
layoutLarge r = Layout {..}
  where
    layout (w, h) x = [(x, (w/2, h/2), s)]
      where s = r * min (w/2) (h/2)
    locate (w, h) _ (x, y)
      | (x - w/2)**2 + (y - h/2)**2 <= s**2 = Just ()
      | otherwise                           = Nothing
      where s = r * min (w/2) (h/2)

translate :: Double -> Double -> [(a, (Double, Double), Double)] -> [(a, (Double, Double), Double)]
translate x' y' = map $ \(a, (x,y), s) -> (a, (x + x', y + y'), s)

layoutGrid :: Layout a b c -> Layout [a] b (Int, c)
layoutGrid (Layout innerLayout innerLocate) = Layout {..}
  where
    layout (w,h) as = concat
        [ translate x y (innerLayout (s,s) a)
        | (i,a) <- zip [0..] as
        , let x = s * fromIntegral (i `mod` per_row)
        , let y = s * fromIntegral (i `div` per_row)
        ]
      where
        per_row = floor (w/(h/4)) :: Integer
        s = w/fromIntegral per_row
    locate (w,h) as (x,y) = do
        let i = floor (x / s)
        let j = floor (y / s)
        let n = i + j * per_row
        let x' = x - s * fromIntegral i
        let y' = y - s * fromIntegral j
        guard (n < length as)
        inner <- innerLocate (s,s) (as !! n) (x', y')
        return (n,inner)
      where
        per_row = floor (w/(h/4)) :: Int
        s = w/fromIntegral per_row
