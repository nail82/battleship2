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

renderBoard :: T.Text
renderBoard =
    let boardCoords =
            [(x,y) | x <- ([0..9] :: [Int]), y <- ([0..9] :: [Int])]
        markers = T.pack $ fmap boardMarker boardCoords
        markers' = T.intersperse ' ' markers
        rows' = [columnHeader] <> markersToRows markers' [] 0

        -- Just a constant for the time being
        boardMarker _ = '_'

        -- |A recursive function to split the single row of markers
        -- |into a list of rows appending the row number to each row.
        markersToRows :: T.Text -> [T.Text] -> Int -> [T.Text]
        -- Base case, we're done.
        markersToRows "" rows _ = reverse rows
        markersToRows ms rows n =
            let (r,ms') = T.splitAt 20 ms
                pad = if n < 9 then "  " else " "
                rowNum = T.append (T.pack $ show (n+1)) pad
            in markersToRows ms' (rowNum <> r:rows) (n+1)

    in T.intercalate "\n" rows'


borderBox :: T.Text -> Widget ()
borderBox title =
    withBorderStyle BS.unicode $
    B.borderWithLabel (txt title) $
    txt $ renderBoard

ui :: Widget ()
ui = C.center (borderBox " Player 1 Shots ")
     <=> B.hBorder
     <=> C.center (borderBox " Player 1 Ships ")

main :: IO ()
main = M.simpleMain ui
