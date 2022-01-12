{-# LANGUAGE OverloadedStrings, TemplateHaskell, FlexibleContexts #-}
module Battle where

import Lens.Micro.TH (makeLenses)
import Lens.Micro
import qualified Data.Text as T

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

data Ship = Ship {
      _design :: Design
    , _coords :: [Coord]
    , _heading :: Heading
    , _shipstate :: ShipState
    } deriving Show

data Game = Game {
-- for each player
--   ships
--   shots
-- turn
-- game mode
    }

$(makeLenses ''Ship)

myShip = Ship {
           _design = Carrier
         , _coords = [(1,1),(2,1),(3,1),(4,1),(5,1)]
         , _heading = South
         , _shipstate = Afloat}

numHoles :: Design -> Int
numHoles d =
    case d of
      Carrier -> 5
      Battleship -> 4
      Sub -> 3
      Destroyer -> 3
      Pt -> 2

offTheBoard :: Coord -> Bool
offTheBoard (r,c)
            | (0 <= r && r < 10) && (0 <= c && c < 10) = False
            | otherwise = True

isCollision :: Ship -> Coord -> Bool
isCollision s c = c `elem` s ^. coords

selectors :: Heading -> (([Int],[Int]) -> [Int], ([Int],[Int]) -> [Int])
selectors h =
    case h of
      North -> (fst, snd)
      South -> (fst, snd)
      _ -> (snd, fst)

zipDims :: Heading -> [Int] -> [Int] -> [(Int, Int)]
zipDims h running constant =
    case h of
      North -> zip running constant
      South -> zip running constant
      _ -> zip constant running

runFn :: Heading -> (Int -> Int -> Int)
runFn h =
    case h of
      South -> (+)
      East -> (+)
      _ -> (-)

emplace :: Heading
         -> Coord
         -> Int
         -> [Coord]
emplace h stern nholes =
    let holes = replicate nholes stern
        (rsel,csel) = selectors h
        fn = runFn h
        zipper = zipDims h
        uz_holes = unzip holes
        running_dim = rsel uz_holes
        ys = [0..(nholes-1)]
        const_dim = csel uz_holes
        running_dim' = zipWith fn running_dim ys
    in zipper running_dim' const_dim



-- Placing errors
-- place the same ship twice (will use the UI to avoid this error)
-- collision with the border
-- collision with another ship
shipEmplacement :: Design -> Heading -> Coord -> Either T.Text Ship
shipEmplacement d h c =
    let holes = numHoles d
        holeFn =
            case h of
              North -> id -- rows decrease
              South -> id -- rows increase
              East -> id  -- cols increase
              West -> id  -- cols decrease
    in undefined

checkShot :: Ship -> Coord -> (Shot, Ship)
checkShot = undefined
