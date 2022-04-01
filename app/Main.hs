{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Brick.Main as M
import GameUtils
import Battle
import UI


main :: IO ()
main = do
  let game = gameInit
  M.simpleMain $ UI.ui game
