{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RecursiveDo #-}
module Kaleidogen where

import Reflex.Dom.FragmentShaderCanvas
import Reflex.Dom

import qualified Data.Text as T
import Data.Maybe
import Text.Read

import Expression
import GLSL

main :: IO ()
main = mainWidgetWithHead (el "title" (text "Kaleidogen")) $
    elAttr "div" ("align" =: "center") $ mdo
        dError <- fragmentShaderCanvas (mconcat
            [ "style"  =: "border: 1px solid black; width: 30%"
            , "width"  =: "1000"
            , "height" =: "1000"
            ]) shader
        el "br" blank
        inp <- textInput $ def
           & textInputConfig_initialValue .~ "1 2"
           & textInputConfig_attributes .~ (return ("style"  =: "width:80%"))
        let nums   = mapMaybe (readMaybe . T.unpack) . T.words <$> (_textInput_value inp)
        let shader = T.pack . toFragmentShader . runProgram <$> nums
        el "br" blank
        elAttr "div" (mconcat
            [ ("style"  =: "width:80%; text-align: left; white-space:pre; font-family:mono")
            ]) $
          dynText (maybe "" id <$> dError)

