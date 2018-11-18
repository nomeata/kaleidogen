{-# LANGUAGE TupleSections #-}
module Layout where

import Control.Monad

type Element b c = ( [(b, (Double, Double), Double)] , (Double,Double) -> Maybe c )

type Layout a b c = a -> (Double, Double) -> Element b c


mapLayout :: (a -> a') -> Layout a' b c -> Layout a b c
mapLayout f innerLayout a = innerLayout (f a)

layoutMaybe :: Layout a b c -> Layout (Maybe a) b c
layoutMaybe _ Nothing = const noElem
layoutMaybe innerLayout (Just a)= innerLayout a

noElem :: Element b c
noElem = ( [], const Nothing )

translate :: Double -> Double -> Element b c -> Element b c
translate x' y' (xs, click) =
    ( map (\(a, (x,y), s) -> (a, (x + x', y + y'), s)) xs
    , click . (\(x,y) -> (x - x', y - y'))
    )

overlay :: Element b c -> Element b c -> Element b c
overlay (xs1, click1) (xs2, click2) = (xs1 ++ xs2, \p -> click1 p `mplus` click2 p)

mapElement :: (c1 -> c2) -> Element b c1 -> Element b c2
mapElement f (xs, click) = (xs, fmap f . click)

layoutAbove :: Layout a b c -> Layout a' b c' -> Layout (a,a') b (Either c c')
layoutAbove innerLayout1 innerLayout2 (a,b) (w,h) =
    overlay
        (mapElement Left $ innerLayout1 a (w,h/2))
        (mapElement Right $ translate 0 (h/2) $ innerLayout2 b (w, h/2))

layoutFullCirlce :: Layout a a ()
layoutFullCirlce a (w, h) = ( xs, click )
  where
    xs = [(a, (w/2, h/2), s) ]
    click (x, y) | (x - w/2)**2 + (y - h/2)**2 <= s**2 = Just ()
                 | otherwise = Nothing
    s = min (w/2) (h/2)

{-
layoutLarge :: Double -> Layout a a ()
layoutLarge r = Layout {..}
  where
    layout (w, h) x = []
      where s = r * min (w/2) (h/2)
    locate (w, h) _ (x, y)
      where s = r * min (w/2) (h/2)

layoutCircular :: Layout a b c -> Layout a' b c' -> Layout (a,[a']) b (Either c (Int,c'))
layoutCircular (Layout innerLayout1 innerLocate1) (Layout innerLayout2 innerLocate2) = Layout {..}
  where
    centric_and_square l radius x =
        translate (- radius) (- radius) $ l (2*radius, 2*radius) x

    layout (w,h) (i,os) = translate (w/2) (h/2) $
        centric_and_square innerLayout1 ir i ++
        concat [translate (       (ir + or) * sin α)
                          (negate (ir + or) * cos α) $
                centric_and_square innerLayout2 or o
        | (n,o) <- zip [0::Int ..] os
        , let α = fromIntegral n * 2 * pi / fromIntegral count - pi/2
        ]
      where
        ir = min (0.25*w) (0.25*h)
        count = max (length os) 10 :: Int
        γ = 2*pi/fromIntegral count
        or = ir * sin (γ/2) / (1 - sin (γ/2))
    locate (w,h) (i,os) (x,y)
        | (x- w/2)**2 + (y - h/2)**2 <= ir**2
        , Just r <- innerLocate1 (2*ir, 2*ir) i (x - w/2 + ir, y - h/2 + ir)
        = Just (Left r)
        | let β = atan2 (x - w/2) (negate (y - h/2)) + pi/2
        , let n = round (β / γ) `mod` count
        , let α = fromIntegral n * 2 * pi / fromIntegral count - pi/2
        , n < length os
        , Just r <- innerLocate2 (2*or, 2*or) (os !! n)
            (x - w/2 -        (ir + or) * sin α + or,
             y - h/2 - negate (ir + or) * cos α + or)
        = Just (Right (n,r))
        | otherwise
        = Nothing
      where
        ir = min (0.25*w) (0.25*h)
        count = max (length os) 10 :: Int
        γ = 2*pi/fromIntegral count
        or = ir * sin (γ/2) / (1 - sin (γ/2))
-}

layoutGrid :: Layout a b c -> Layout [a] b (Int, c)
layoutGrid _ _ (w,h) | w == 0 || h == 0 = noElem
layoutGrid innerLayout as (w,h) =
    foldr overlay noElem
        [ mapElement (n,) $ translate x y $ innerLayout a (s,s)
        | (n,a) <- zip [0..] as
        , let i' = n `mod` per_double_row
        , let j' = n `div` per_double_row

        -- Move to upper half-row if needed
        , let (i,j) | i' < per_row = (i',2 * j')
                    | otherwise    = (i' - per_row, 2 * j' + 1)

        , let x | even j    = s * fromIntegral i
                | otherwise = w - s * fromIntegral i - s - s/2
        , let y = h - row_height * fromIntegral j - s
        ]
      where
        max_size = min (w/6) (h/2)
        min_per_row = ceiling (w / max_size)
        per_row = head
            [ i | i <- [min_per_row..]
            , let s' = w/fromIntegral i
            , let rows = 1 + floor ((h - s') / (sqrt 3 / 2 * s'))
            , length as <= i * rows - rows`div`2
            ]
        per_double_row = per_row + per_row - 1
        s = w/fromIntegral per_row
        row_height = sqrt 3 / 2 * s
