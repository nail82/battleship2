{-# LANGUAGE OverloadedStrings #-}

module Main where

import Battle
import UI
import qualified Brick.Main as M

main :: IO ()
main = M.simpleMain UI.ui
