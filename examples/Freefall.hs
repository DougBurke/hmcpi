{-# LANGUAGE RecordWildCards #-}

{-
License:

This code is placed in the Public Domain.

Author:

Douglas Burke (dburke.gw@gmail.com)

Usage:

  ./freefall [--debug] [<height>]

Example program showing how to send commands to MineCraft on
Raspberry Pi.

The program increases the user's current height by the
given argument, which defaults to 100. There is no sanity
check to ensure that the height is positive.
-}

module Main where

import Control.Monad (when)
import Control.Monad.IO.Class

import Data.MineCraft.Pi.Block
import Data.MineCraft.Pi.Other
import Data.MineCraft.Pi.Player
import Data.MineCraft.Pi.Types

-- Most users would import @runMCPI@ from
-- "Network.MineCraft.Pi.Client", but I want to allow
-- debug messages.
import Network.MineCraft.Pi.Client.Internal (MCPI, runMCPI')

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO

import Utils (checkForDebug, maybeRead, printVersion)

moveUser :: Int -> MCPI ()
moveUser j = do
    liftIO $ putStrLn "Connected to MineCraft"
    Pos {..} <- getPlayerTile
    chatPost $ "Jumping from " ++ show _z ++ " to " ++ show (_z + j)
    let newPos = Pos _x _y (_z + j)
    bType <- getBlock newPos
    if bType == air
      then do
        setPlayerTile newPos
        bType_ <- getBlock $ Pos _x _y (_z + j - 1)
        when (bType_ == air) (chatPost "Weeeeeeeeee!!!!!!")

      else liftIO $ putStrLn ("Awwwww: jump would end in " ++ 
                              showBlock bType) 

    liftIO $ putStrLn "Exiting from MineCraft"

usage :: IO ()
usage = do
    progName <- getProgName
    hPutStrLn stderr $ "usage: " ++ progName ++ " [--debug] [<jump>]"
    hPutStrLn stderr   "\n       The default jump is 100 (blocks)."
    exitFailure

main :: IO ()
main = do
  args <- getArgs
  let (dbg, args') = checkForDebug args
  when dbg printVersion
  case args' of
    [] -> runMCPI' dbg (moveUser 100)
    [jstr] -> case maybeRead jstr of
                  Just j -> runMCPI' dbg (moveUser j)
                  _ -> usage
    _ -> usage

