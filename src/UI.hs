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

-- Board Rendering -------------------------------------------------------------

-- Row and column labels
columnHeader :: T.Text
columnHeader = T.concat ["   ", T.intersperse ' ' $ T.pack ['A'..'J']]

rowHeader :: T.Text
rowHeader =
    let rows = fmap (T.pack . show) ([1..10] :: [Int])
    in T.intercalate "\n" rows

-- Reduction function for picking out a ship marker from a list of ships
shipMarker :: Coord -> Ship -> Char -> Char
-- Short circuit if a shot marker has been set
shipMarker _ _ 'x' = 'x'
shipMarker _ _ 'o' = 'o'
shipMarker c s z =
    if c `elem` s ^. coords then
        designMarker $ s ^. design
    else
        z

-- Set a shot marker, if applicable
shotMarker :: Coord -> [Shot Coord] -> Char
shotMarker c shots
    | Miss c `elem` shots = 'o'
    | Hit c `elem` shots = 'x'
    | otherwise = '_'

boardMarker :: ([Shot Coord], [Ship]) -> Coord -> Char
boardMarker (shots, ships) c =
    let s = shotMarker c shots
    in foldr (shipMarker c) s ships

renderBoard :: (Coord -> Char) -> T.Text
renderBoard markerFn =
    let boardCoords =
            [(x,y) | x <- ([0..9] :: [Int]), y <- ([0..9] :: [Int])]
        markers = T.pack $ fmap markerFn boardCoords
        markers' = T.intersperse ' ' markers
        rows' = [columnHeader] <> markersToRows markers' [] 0

        -- |A recursive function to split the single row of markers
        -- |into a list of rows, appending the row number to each row.
        markersToRows :: T.Text -> [T.Text] -> Int -> [T.Text]
        -- Base case, we're done.
        markersToRows "" rows _ = reverse rows
        markersToRows ms rows n =
            let (r,ms') = T.splitAt 20 ms
                pad = if n < 9 then "  " else " "
                rowNum = T.append (T.pack $ show (n+1)) pad
            in markersToRows ms' (rowNum <> r:rows) (n+1)

    in T.intercalate "\n" rows'

-- Resolve the board marker function.
-- It's a function of the board type and player.

selectShots :: Board -> Game -> (Coord -> Char)
-- Own shots, no ships (don't show the other player's ships)
selectShots ShotBoard game =
    case game ^. turn of
      P1 -> boardMarker (game ^. p1shots, [])
      _ -> boardMarker (game ^. p2shots, [])

-- Other player's shots and own ships
selectShots ShipBoard game =
    case game ^. turn of
      P1 -> boardMarker (game ^. p2shots, game ^. p1ships)
      _ -> boardMarker (game ^. p1shots, game ^. p2ships)

-- Brick Work ------------------------------------------------------------------

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
