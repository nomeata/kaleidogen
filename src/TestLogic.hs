{-# OPTIONS_GHC -Wno-orphans #-}
module TestLogic (canReconstruct) where

import qualified Data.Map as M
import Test.QuickCheck

import Logic
import Drag (ClickEvent(..))
import Mealy
import PresentationCmds

nextActions :: AppState -> [Event]
nextActions as =
    [ ClickEvent (Click d)     | d <- ds ] ++
    [ ClickEvent (BeginDrag d) | d <- ds ] ++
    [ ClickEvent (DragOn d)    | d <- ds ] ++
    [ ClickEvent (DragOff d)   | d <- ds ] ++
    [ ClickEvent EndDrag ] ++
    [ ClickEvent CancelDrag ] ++
    [ Delete ] ++
    [ Anim ] ++
    [ ClickEvent OtherClick ]
  where
    ds = M.elems (dnas as)

instance Arbitrary AppState where
  arbitrary = sized $ go
    where
      m = logicMealy 1
      go :: Int -> Gen AppState
      go 0 = pure $ initial m
      go n = do
          s <- go (n-1)
          e <- elements (nextActions s)
          pure $ fst $ handle m s e

canReconstruct :: AppState -> Property
canReconstruct as = forAll (elements (nextActions as)) $ \e ->
    let (as', es) = handle m as e in
    let m1 = finalPos $ reconstruct m as ++ es in
    let m2 = finalPos $ reconstruct m as' in
    let expl = unlines [p m1, "vs.", p m2] in
    counterexample expl (m1 == m2)
  where
    m = logicMealy 1
    p aps = unlines [ show x ++ ": " ++ show y | (x,y) <- M.toList aps ]


finalPos :: Ord k => Cmds k a -> M.Map k a
finalPos = foldl go M.empty
  where
    go m (k, SummonAt ap) = M.insert k ap m
    go m (k, MoveTo   ap) = M.insert k ap m
    go m (k, FadeOut  _)  = M.delete k m
    go m (_, Animate)     = m
    go m (k, Remove)      = M.delete k m


