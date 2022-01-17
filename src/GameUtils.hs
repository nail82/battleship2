{-# LANGUAGE OverloadedStrings #-}
module GameUtils where

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

isCollision :: [Coord] -> [Ship] -> Bool
isCollision current other = False

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
shipEmplacement d h stern =
    let nholes = numHoles d
        holes = emplace h stern nholes
        offboard = L.any offTheBoard holes
        collision = isCollision holes []
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

-- Game init --

-- Take shots --

--checkshot :: Ship -> Coord -> (Shot, Ship)
checkShot = undefined
