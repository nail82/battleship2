{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B

import Brick.Types
    ( Widget
    )

import Brick.Widgets.Core
    (
     (<=>)
    , hBox
    , vLimit
    , withBorderStyle
    , str
    , txt
    )

columnHeader :: T.Text
columnHeader = T.concat ["   ", T.intersperse ' ' $ T.pack ['A'..'J']]

rowHeader :: T.Text
rowHeader =
    let rows = fmap (T.pack . show) ([1..10] :: [Int])
    in T.intercalate "\n" rows

borderBox :: Widget ()
borderBox =
    withBorderStyle BS.unicode $
    B.borderWithLabel (str " Player 1 ") $
    txt $ T.concat [columnHeader, "\n", rowHeader]

ui :: Widget ()
ui = C.center $ hBox [borderBox]

main :: IO ()
main = M.simpleMain ui
