{-# LANGUAGE OverloadedStrings #-}

module UI where

import Lens.Micro
import qualified Data.Text as T
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import Battle

import Brick.Types
    ( Widget
    )

import Brick.Widgets.Core
    (
     (<=>)
    , (<+>)
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


boardMarker :: ([Shot (Int, Int)], [Ship]) -> Coord -> Char
boardMarker (shots, ships) c = '_'
             -- |
             -- |
             -- |
             -- |
             -- |
             -- | otherwise = '_'

renderBoard :: (Coord -> Char) -> T.Text
renderBoard markerFn =
    let boardCoords =
            [(x,y) | x <- ([0..9] :: [Int]), y <- ([0..9] :: [Int])]
        markers = T.pack $ fmap markerFn boardCoords
        markers' = T.intersperse ' ' markers
        rows' = [columnHeader] <> markersToRows markers' [] 0

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

selectShots :: Board -> Game -> (Coord -> Char)
-- Own shots, no ships
selectShots ShotBoard game =
    case game ^. turn of
      P1 -> boardMarker (game ^. p1shots, [])
      _ -> boardMarker (game ^. p2shots, [])

-- Other player shots and own ships
selectShots ShipBoard game =
    case game ^. turn of
      P1 -> boardMarker (game ^. p2shots, game ^. p1ships)
      _ -> boardMarker (game ^. p1shots, game ^. p2ships)

borderBox :: (Coord -> Char) -> T.Text -> Widget ()
borderBox markerFn title =
    withBorderStyle BS.unicode $
    B.borderWithLabel (txt title) $
    txt $ renderBoard markerFn

ui :: Game -> Widget ()
ui game =
    let player = T.pack $ show (game ^. turn)
        shottitle = player <> "Shots "
        shiptitle = player <> "Ships "

    in C.center (borderBox (selectShots ShotBoard game) shottitle)
           <=> B.hBorder
           <=> C.center (borderBox (selectShots ShipBoard game) shiptitle)
