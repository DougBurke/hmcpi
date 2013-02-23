{-# LANGUAGE RecordWildCards #-}

{-
License:

This code is placed in the Public Domain.

Author:

Douglas Burke (dburke.gw@gmail.com)

Usage:

  ./flatten [--debug] [<radius>]

Convert all blocks within the given distance of the player,
and at a height one below that of the player, to gold.
Any blocks above this are removed.

If radius is not given then it defaults to 5. There is no
sanity check to ensure that radius is positive.

It could check that we are not about to place blocks onto
air/water.
-}

module Main where

import Control.Monad (forM_, when)
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

-- | Given x0,y0 and a radius, return all the block positions within
--   the circle. We use the coordinate of each block to determine
--   whether it is in or out.
--
--   Could be much-more efficient.
circleCoords :: Int -> Int -> Int -> [(Int, Int)]
circleCoords x0 y0 r = 
   let r2 = r * r
       isIn (a,b) = (a-x0)*(a-x0) + (b-y0)*(b-y0) <= r2
   in filter isIn [(i,j) | i <- [x0-r .. x0+r], j <- [y0-r .. y0+r]]
 
-- | Flatten the area surrounding the user to a distance
--   of r (radius) blocks. The ground is set to the given
--   block type.
--
--   TODO: shoud check that the blocks are not going onto
--   water/over air/replacing ice so that they do not sink,
--   but it may not be worth the effort.
--
flattenArea :: BlockType -> Int -> MCPI ()
flattenArea bType r = do
    liftIO $ putStrLn "Connected to MineCraft"
    Pos {..} <- getPlayerTile
    liftIO $ putStrLn $ "User is at x,y=" ++ show _x ++ "," ++ show _y ++
                        " and a height of " ++ show _z

    forM_ (circleCoords _x _y r) $ \(i,j) -> do
        -- clear everything above this point
        -- it depends on whether height means max z containing a
        -- block, or the max connected z (probably the latter)
        --
        let p0 = Pos i j $ _z - 1
        h <- getHeight p0
        when (h >= _z) $ setBlocks 
                          (Pos i j _z)
                          (Pos i j h)
                          air

        setBlock p0 bType

    liftIO $ putStrLn "Exiting from MineCraft"

usage :: IO ()
usage = do
    progName <- getProgName
    hPutStrLn stderr $ "Usage: " ++ progName ++ " [--debug] [<radius>]"
    exitFailure

main :: IO ()
main = do
  args <- getArgs
  let (dbg, args') = checkForDebug args
  when dbg printVersion
  case args' of
    [] -> runMCPI' dbg (flattenArea goldOre 5)
    [rstr] -> case maybeRead rstr of
                  Just r -> runMCPI' dbg (flattenArea goldOre r)
                  _ -> usage
    _ -> usage

