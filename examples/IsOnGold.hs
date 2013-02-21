{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (when)

import Data.MineCraft.Pi.Block 
import Data.MineCraft.Pi.Player
import Data.MineCraft.Pi.Types

import Network.MineCraft.Pi.Client

-- | Returns @True@ if the player is standing on gold ore.
isOnGold :: IO Bool
isOnGold = runMCPI $ do
    Pos {..} <- getPlayerTile
    bType <- getBlock $ Pos _x _y (_z-1)
    return (bType == goldOre)

main :: IO ()
main = do
    flag <- isOnGold
    when flag $ putStrLn "Look down!"
