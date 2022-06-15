module Layout where

type Pos = (Double, Double)
type WindowSize = (Double, Double)
type PosAndScale = (Pos, Double)

noPas :: PosAndScale
noPas = ((0,0),0)

type Layout a = WindowSize -> a -> PosAndScale

translate :: (Double, Double) -> PosAndScale -> PosAndScale
translate (x', y') ((x,y),s) = ((x + x', y + y'), s)

topHalf :: Layout a -> Layout a
topHalf l (w,h) = l (w,h/2)

bottomHalf :: Layout a -> Layout a
bottomHalf l (w,h) = translate (0, h/2) . l (w,h/2)

padding :: Layout a -> Layout a
padding l (w,h) = translate (p, p) . l (w-2*p,h-2*p)
  where p = min (h/20) (w/20)

layoutFullCirlce :: Layout ()
layoutFullCirlce (w, h) = \() -> pas
  where
    pas = ((w/2, h/2), s)
    s = min (w/2) (h/2)

layoutCenterDot :: Layout ()
layoutCenterDot (w, h) = \() -> ((w/2, h/2), 0)

layoutGrid :: Int -> Layout Int
layoutGrid _ (w,h) | w == 0 || h == 0 = const ((0,0),0)
layoutGrid count (w,h) =
    \n -> translate (toPos n) ((s/2, s/2), s/2)
  where
    max_size = min (w/6) (h/2)
    min_per_row = ceiling (w / max_size)
    per_row = head
        [ i | i <- [min_per_row..]
        , let s' = w/fromIntegral i
        , let rows = 1 + floor ((h - s') / (sqrt 3 / 2 * s'))
        , count <= i * rows - rows`div`2
        ]
    per_double_row = per_row + per_row - 1
    s = w/fromIntegral per_row
    row_height = sqrt 3 / 2 * s

    toPos n = (x,y)
      where
        i' = n `mod` per_double_row
        j' = n `div` per_double_row

        -- Move to upper half-row if needed
        (i,j) | i' < per_row = (i',2 * j')
              | otherwise    = (i' - per_row, 2 * j' + 1)

        x | even j    = s * fromIntegral i
          | otherwise = w - s * fromIntegral i - s - s/2
        y = h - row_height * fromIntegral j - s
