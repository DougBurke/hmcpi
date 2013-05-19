{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)

import Data.MineCraft.Pi.Block
import Data.MineCraft.Pi.Player
import Data.MineCraft.Pi.Types
import Data.MineCraft.Pi.Other

-- import Network.MineCraft.Pi.Client (MCPI, runMCPI)
import Network.MineCraft.Pi.Client.Internal (MCPI, runMCPI')

import System.Environment (getProgName)
import System.Exit (exitFailure)
import System.IO

-- | Could also store the bounding box for each option.
data Shape =
  BlockList BlockType [IPos]
  | BlockCuboid BlockType IPos IPos
    
type Shapes = [Shape]    

-- | Given a center and width, return the coordinates  
--   of the first and last item.
limits :: Int -> Int -> (Int, Int)
limits c w =  
  let (hw, d) = w `divMod` 2
  in (c - hw, c + hw + d)

-- | If the width is even, the cube will extend from    
--   @center - width/2@ to @center + width/2@.
makeRectangle :: 
  BlockType -- ^ block type
  -> IPos   -- ^ center of rectangle
  -> Int    -- ^ width in x
  -> Int    -- ^ width in y
  -> Int    -- ^ width in z
  -> Shape 
makeRectangle bt Pos {..} wx wy wz =
  let (x1,x2) = limits _x wx
      (y1,y2) = limits _y wy
      (z1,z2) = limits _z wz
      p1 = Pos x1 y1 z1
      p2 = Pos x2 y2 z2
      
  in BlockCuboid bt p1 p2

-- | If the width is even, the cube will extend from    
--   @center - width/2@ to @center + width/2@.
makeCube :: 
  BlockType -- ^ block type
  -> IPos   -- ^ center of cube
  -> Int    -- ^ width of cube
  -> Shape 
makeCube bt pos w = makeRectangle bt pos w w w

-- | Note that we convert all the other blocks in the
--   cube to air.
makePyramid ::
  BlockType   -- ^ block type
  -> IPos     -- ^ center of the base
  -> Int      -- ^ width of base (also the height)
  -> Shapes
makePyramid bt Pos {..} w =
  let (hw,_) = w `divMod` 2
      pMid = Pos _x _y (_z + hw)
      toP l = makeRectangle bt (Pos _x _y (_z+l)) (w-2*l) (w-2*l) 0
      pyramid = map toP [0..hw]
  in makeCube air pMid w : pyramid

renderShape :: Shapes -> MCPI ()
renderShape = 
  let rs (BlockList bt pos) = mapM_ (\p -> setBlock p bt) pos
      rs (BlockCuboid bt p1 p2) = setBlocks p1 p2 bt
  in mapM_ rs

frob :: BlockType -> MCPI ()
frob bt = do
  pos@Pos {..} <- getPlayerTile
  let shape = makePyramid bt pos 12
  setPlayerTile $ Pos (_x+20) (_y+20) _z
  renderShape shape
  setPlayerTile $ Pos _x _y (_z+40)
  
usage :: IO ()
usage = do
    progName <- getProgName
    hPutStrLn stderr $ "Usage: " ++ progName
    exitFailure

-- | Move the player by the given coordinates, which
--   are relative to the current location. The return
--   value is the new coordinate of the player, if  
--   that position contains air, otherwise the player
--   is not moved.
--
movePlayer ::
  Int     -- ^ Change in X
  -> Int  -- ^ Change in Y
  -> Int  -- ^ Change in Z
  -> MCPI (Maybe (Pos Int))
movePlayer dx dy dz = do
  Pos {..} <- getPlayerTile
  let nPos = Pos (_x + dx) (_y + dy) (_z + dz)
  btype <- getBlock nPos
  if btype == air
    then setPlayerTile nPos >> return (Just nPos)
    else return Nothing
         
showBlocksAsWall :: MCPI ()
showBlocksAsWall = do
  Pos {..} <- getPlayerTile
  let x1 = _x - 128
      y1 = _y + 20      
  setBlocks (Pos x1 y1 (_z-1)) (Pos (x1+255) (y1+4) (_z+1)) $ sandstone
  
  forM_ [0..255] $ \ctr ->
    setBlocks 
       (Pos (x1+ctr) y1 _z)
       (Pos (x1+ctr) (y1+4) (_z+16))
       $ BlockType $ fromIntegral ctr

showBlocksAsFloor :: MCPI ()
showBlocksAsFloor = do
  Pos {..} <- getPlayerTile
  let w = 4
      hw = 8 * w
      x1 = _x - hw
      y1 = _y - hw      
      x2 = x1 + 2 * hw
      y2 = y1 + 2 * hw
      z = _z - 1
  setBlocks (Pos x1 y1 (z-1)) (Pos x2 y2 (z-1)) $ sandstone
  
  forM_ [0..15] $ \j ->
    let ys = y1 + j * w
        ye = ys + w - 1
        ctrBase = j * 16
    in forM_ [0..15] $ \i ->
      let xs = x1 + i * w
          xe = xs + w - 1
          ctr = fromIntegral $ i + ctrBase
      in setBlocks
         (Pos xs ys z)
         (Pos xe ye z)
         $ BlockType ctr

-- TODO: report only when block data differs
reportBlock :: MCPI ()
reportBlock = 
  let go oPos = do
        liftIO $ threadDelay 1000000
        nPos <- getPlayerTile
        if nPos == oPos
          then go oPos
          else do
            let bPos = Pos { _x = _x nPos
                           , _y = _y bPos
                           , _z = _z nPos - 1
                           }
            (bType, bData) <- getBlockData bPos
            chatPost $ "Block: " ++ showBlock bType
            chatPost $ "     : " ++ show bData
            go nPos
            
  in getPlayerTile >>= go
  
main :: IO ()
main = runMCPI' True reportBlock

