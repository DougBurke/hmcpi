{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (when)

import Data.MineCraft.Pi.Block 
import Data.MineCraft.Pi.Player
import Data.MineCraft.Pi.Other
import Data.MineCraft.Pi.Types

import Network.MineCraft.Pi.Client

-- | Move the player by 10 tiles in the X direction,
--   if it is not filled.
movePlayer :: MCPI ()
movePlayer = do
    Pos {..} <- getPlayerTile
    let newPos = Pos (_x+10) _y _z
    bType <- getBlock newPos
    when (bType == air) $ do
      setPlayerTile newPos
      chatPost "*jump*"

main :: IO ()
main = runMCPI movePlayer
