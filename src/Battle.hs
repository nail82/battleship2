{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Battle where

import Lens.Micro.TH (makeLenses)

type Coord = (Int, Int)

data ShipState = Afloat | Sunk deriving Show

data Design = Carrier
            | Battleship
            | Sub
            | Destroyer
            | Pt
              deriving (Eq, Show)

designMarker :: Design -> Char
designMarker Carrier = 'C'
designMarker Battleship = 'B'
designMarker Sub = 'S'
designMarker Destroyer = 'D'
designMarker Pt = 'P'

data Heading = North | South | East | West deriving (Eq, Show)

data Shot a = Hit a
          | Miss a
            deriving Show

data Board = ShotBoard | ShipBoard deriving Eq

data Turn = P1 | P2



data GameMode = Emplacement
              | Fire
              | Switch
              | Over
                deriving (Eq, Show)

data Ship = Ship { _design :: Design
                 , _coords :: [Coord]
                 , _heading :: Heading
                 , _shipstate :: ShipState
                 } deriving Show

data Game = Game { _p1ships :: [Ship]
                 , _p2ships :: [Ship]
                 , _p1shots :: [Shot (Int, Int)]
                 , _p2shots :: [Shot (Int, Int)]
                 , _turn :: Turn
                 , _mode :: GameMode
                 }

-- Instances -------------------------------------------------------------------

instance Show Turn where
    show P1 = " Player 1 "
    show P2 = " Player 2 "

instance (Eq a) => Eq (Shot a) where
    (==) (Hit h) (Miss m) = h == m
    (==) (Miss m) (Hit h) = h == m
    (==) (Hit h1) (Hit h2) = h1 == h2
    (==) (Miss m1) (Miss m2) = m1 == m2


$(makeLenses ''Ship)
$(makeLenses ''Game)
