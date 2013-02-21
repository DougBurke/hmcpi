--------------------------------------------------------------------------------
-- |
--  Module      :  Player
--  License     :  Public Domain
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  Haskell 98
--
-- Players in MineCraft.
--
-- *Note* this module currently does not support multiple players.
--
--------------------------------------------------------------------------------

module Data.MineCraft.Pi.Player
    (
      -- * Queries
      getPlayerTile
    , getPlayerPos

      -- * Commands
    , setPlayerTile
    , setPlayerPos
    ) where

import Control.Monad (liftM)

import Data.MineCraft.Pi.Types

import Network.MineCraft.Pi.Client
import Network.MineCraft.Pi.Client.Internal

-- | Where is the user. See also `getPlayerPos`.
getPlayerTile :: MCPI IPos 
getPlayerTile = fromMC `liftM` query "player.getTile" []

-- | Move the user. See also `setPlayerPos`.
setPlayerTile :: IPos -> MCPI ()
setPlayerTile pos = command "player.setTile" [toMC pos]

-- | Where is the user. See also `getPlayerPos`.
getPlayerPos :: MCPI FPos
getPlayerPos = fromMC `liftM` query "player.getPos" []

-- | Move the user. See also `setPlayerPos`.
setPlayerPos :: FPos -> MCPI ()
setPlayerPos pos = command "player.setPos" [toMC pos]

