module Layout where

import Control.Monad
import Data.Foldable (asum)

type PosAndScale = ((Double,Double), Double)
type ClickPos = (Double, Double)
type ClickFun a = ClickPos -> Maybe a

noPas :: PosAndScale
noPas = ((0,0),0)

type Element a = (a -> PosAndScale, ClickFun a)
type Layout a = (Double, Double) -> Element a

translate :: (Double, Double) -> Element a -> Element a
translate (x', y') (pas, click) =
    ( (\((x,y), s) -> ((x + x', y + y'), s)) . pas
    , click . (\(x,y) -> (x - x', y - y'))
    )
noElem :: Element a
noElem = (const ((0,0),0), const Nothing)

overlay :: Element a -> Element b -> Element (Either a b)
overlay (pas1, click1) (pas2, click2) =
    ( either pas1 pas2
    , \p -> (Left <$> click1 p) `mplus` (Right <$> click2 p)
    )

layoutAbove :: Layout a -> Layout b -> Layout (Either a b)
layoutAbove innerLayout1 innerLayout2 (w,h) =
    overlay
        (innerLayout1 (w,h/2))
        (translate (0, h/2) $ innerLayout2 (w, h/2))

layoutFullCirlce :: Layout ()
layoutFullCirlce (w, h) = ( \() -> pas, click )
  where
    pas = ((w/2, h/2), s)
    click (x, y) | (x - w/2)**2 + (y - h/2)**2 <= s**2 = Just ()
                 | otherwise = Nothing
    s = min (w/2) (h/2)

layoutGrid :: Int -> Layout () -> Layout Int
layoutGrid _ _ (w,h) | w == 0 || h == 0 = noElem
layoutGrid count innerLayout (w,h) =
    ( \n -> fst (layouts !! n) ()
    , \p -> asum $ zipWith (\n (_,click) -> n <$ click p) [0..] layouts
    )
  where
    layouts :: [Element ()]
    layouts = [ translate (toPos n) (innerLayout (s,s)) | n <- [0..count-1] ]

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
