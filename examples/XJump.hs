{-# LANGUAGE RecordWildCards #-}

{-

Usage:

  ./xjump [--debug]

-}

module Main where

import Control.Monad (when, unless)

import Data.MineCraft.Pi.Block 
import Data.MineCraft.Pi.Player
import Data.MineCraft.Pi.Other
import Data.MineCraft.Pi.Types

-- Most users would import @runMCPI@ from
-- "Network.MineCraft.Pi.Client", but I want to allow
-- debug messages.
import Network.MineCraft.Pi.Client.Internal (MCPI, runMCPI')

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO

import Utils (printVersion, checkForDebug)

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

usage :: IO ()
usage = do
  pName <- getProgName
  hPutStrLn stderr $ "Usage: " ++ pName ++ " [--debug]"
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  let (dbg, args') = checkForDebug args
  unless (null args') usage
  when dbg printVersion
  runMCPI' dbg movePlayer
