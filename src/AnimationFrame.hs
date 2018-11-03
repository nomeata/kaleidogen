{-# LANGUAGE RecursiveDo #-}
module AnimationFrame where

import Control.Monad.IO.Class
import Control.Monad.Fix

import Reflex.Dom
import GHCJS.DOM
import GHCJS.DOM.Types hiding (Text, Event)
import GHCJS.DOM.Window
import GHCJS.DOM.RequestAnimationFrameCallback


getAnimationFrameE :: (TriggerEvent t m, MonadJSM m) => m (Event t Double)
getAnimationFrameE = mdo
  (ev, trigger) <- newTriggerEvent
  Just win <- currentWindow
  let go d = do
      liftIO $ trigger d
      cb <- newRequestAnimationFrameCallback go
      _ <- requestAnimationFrame win cb
      return ()
  liftJSM $ go 0.0
  return ev

getAnimationFrameD :: (Reflex t, MonadHold t m, MonadFix m, TriggerEvent t m, MonadJSM m) => m (Dynamic t Double)
getAnimationFrameD = holdDyn 0 =<< getAnimationFrameE
