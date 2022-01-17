{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}
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

data Heading = North | South | East | West deriving (Eq, Show)

data Shot = Hit | Miss deriving Show

data Turn = P1 | P2 deriving Show

data GameMode = Emplacement
              | Fire
              | Switch
              | Over
                deriving (Eq, Show)

data Ship = Ship {
      _design :: Design
    , _coords :: [Coord]
    , _heading :: Heading
    , _shipstate :: ShipState
    } deriving Show

data Game = Game {
      _p1ships :: [Ship]
    , _p2ships :: [Ship]
    , _p1shots :: [Coord]
    , _p2shots :: [Coord]
    , _turn :: Turn
    , _mode :: GameMode
    }

$(makeLenses ''Ship)
$(makeLenses ''Game)
