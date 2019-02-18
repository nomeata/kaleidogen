module KaleidogenAndroid where

import qualified KaleidogenSDL
import Android.HaskellActivity

main :: IO ()
main = continueWithCallbacks $ def
    { _activityCallbacks_onStart = KaleidogenSDL.main
    }
