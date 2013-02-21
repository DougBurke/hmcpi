--------------------------------------------------------------------------------
-- |
--  Module      :  Types
--  License     :  Public Domain
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  Haskell 98
--
-- Useful data types. The block types should probably move to
-- "Data.MineCraft.Pi.Block".
--
--------------------------------------------------------------------------------

module Data.MineCraft.Pi.Types 
   (
     Pos(..)
   , IPos
   , FPos

   , BlockType(..)
   , BlockData(..)

   ) where

import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Word (Word16)

import Network.MineCraft.Pi.Client

-- | Represent the position of an entity. Note that we use
--   @(X,Y,Z)@ order rather than @(X,Z,Y)@ used by the MineCraft API.
--
--   This *should* be replaced by one of the \"standard\" Haskell
--   3D vector types.
--
data Pos a = Pos { _x :: a, _y :: a, _z :: a }
   deriving (Eq, Show)

type IPos = Pos Int
type FPos = Pos Float

instance FromMineCraft a => FromMineCraft (Pos a) where
  fromMC s = let toks = map fromMC $ splitOn "," s
             in case toks of
                    (x:z:y:[]) -> Pos x y z
                    _ -> error $ "*Expected 3 values* " ++ s

instance ToMineCraft a => ToMineCraft (Pos a) where
  toMC (Pos x y z) = intercalate "," $ map toMC [x,z,y]

-- | Represent a block.
--
--   We should probably combine `BlockType` and `BlockData`.
--   I have not looked to see whether it is worth using
--   an integer (as I currently do) or just an enumerated
--   type (e.g. if there are large ranges of the range 0 to 1023
--   that do not represent a valid block).
--
--   Use `Data.MineCraft.Pi.Block.showBlock` for a more readable
--   way to convert to a @String@.
newtype BlockType = BlockType Word16 
  deriving (Eq, Ord, Show)

-- | Data on a block.
newtype BlockData = BlockData Int
  deriving (Eq, Ord, Show)

{- Looks like this range includes entities and other items than just
blocks.
instance Bounded BlockType where
  minBound = BlockType 0
  maxBound = BlockType 1023
-}

instance ToMineCraft BlockType where
  toMC (BlockType bt) = show bt

instance FromMineCraft BlockType where
  fromMC val = let i = eRead val
               in if i >= 0 && i < 1024
                  then BlockType i
                  else error $ "*invalid block type: " ++ val

instance ToMineCraft BlockData where
  toMC (BlockData bd) = show bd

instance FromMineCraft BlockData where
  fromMC = BlockData . eRead

