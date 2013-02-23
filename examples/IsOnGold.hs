{-# LANGUAGE RecordWildCards #-}

{-

Usage:

  ./isongold [--debug]

-}

module Main where

import Control.Monad (unless, when)

import Data.MineCraft.Pi.Block 
import Data.MineCraft.Pi.Player
import Data.MineCraft.Pi.Types

-- Most users would import @runMCPI@ from
-- "Network.MineCraft.Pi.Client", but I want to allow
-- debug messages.
import Network.MineCraft.Pi.Client.Internal (runMCPI')

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO

import Utils (printVersion, checkForDebug)

-- | Returns @True@ if the player is standing on gold ore.
--
--   The input flag controls whether debug messages are
--   displayed.
isOnGold :: Bool -> IO Bool
isOnGold flag = runMCPI' flag $ do
    Pos {..} <- getPlayerTile
    bType <- getBlock $ Pos _x _y (_z-1)
    return (bType == goldOre)

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
    flag <- isOnGold dbg
    when flag $ putStrLn "Look down!"
