{-# LANGUAGE OverloadedStrings #-}
module GameUtils where

import Lens.Micro
import qualified Data.List as L
import qualified Data.Text as T
import Battle

-- Ship Emplacement ------------------------------------------------------------

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

isCollision :: [Coord] -> Ship -> Bool
isCollision current other =
    let ocoords = other ^. coords
    in L.any (`elem` ocoords) current

selectors :: Heading -> (([Int],[Int]) -> [Int], ([Int],[Int]) -> [Int])
selectors h =
    case h of
      North -> (fst, snd)
      South -> (fst, snd)
      _ -> (snd, fst)

zipDims :: Heading -> [Int] -> [Int] -> [(Int, Int)]
zipDims h =
    case h of
      North -> zip
      South -> zip
      _ -> flip zip

runFn :: Heading -> (Int -> Int -> Int)
runFn h =
    case h of
      South -> (+)
      East -> (+)
      _ -> (-)

-- Emplace a ship heading in a particular direction.
-- I'm quite sure there is a cleaner linear algebra solution.
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

shipEmplacement :: [Ship] -> Design -> Heading -> Coord -> Either T.Text Ship
shipEmplacement ships d h stern =
    let nholes = numHoles d
        holes = emplace h stern nholes
        offboard = L.any offTheBoard holes
        collision = L.any (isCollision holes) ships
    in case (offboard,collision) of
         (False, False) ->
             Right Ship { _design = d
                  , _coords = holes
                  , _heading = h
                  , _shipstate = Afloat}
         (True, _) ->
             Left "Emplacement is off the board"
         (_, True) ->
             Left "Emplacement collides with another ship"



-- Shots -----------------------------------------------------------------------

checkShot :: Coord -> [Ship] -> Shot (Int, Int)
checkShot c ships = undefined


-- Game init -------------------------------------------------------------------

gameInit :: Game
gameInit = Game {
             _p1ships = []
           , _p2ships = []
           , _p1shots = []
           , _p2shots = []
           , _turn = P1
           , _mode = Emplacement
           }
