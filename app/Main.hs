{-# LANGUAGE OverloadedStrings #-}

module Main where

--import qualified Graphics.Vty as V
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B

import Brick.Types
    ( Widget
    )

import Brick.Widgets.Core
    (
     vLimit
    , withBorderStyle
    , str
    , txt
    )


ui :: Widget ()
ui =
    withBorderStyle BS.unicode $
    B.borderWithLabel (str " Player 1 ") $
    vLimit 10 $
    C.vCenter $
    txt $ " Ship Board "


main :: IO ()
main = M.simpleMain ui
