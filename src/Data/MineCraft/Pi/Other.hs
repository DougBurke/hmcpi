{-# LANGUAGE RecordWildCards #-}

--------------------------------------------------------------------------------
-- |
--  Module      :  Other
--  License     :  Public Domain
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  RecordWildCards
--
-- A mixture of routines.
--
--------------------------------------------------------------------------------

module Data.MineCraft.Pi.Other
    (
      -- * Queries
      getHeight

      -- * Commands
    , chatPost
    ) where

import Control.Monad (liftM)

import Data.MineCraft.Pi.Types

import Network.MineCraft.Pi.Client
import Network.MineCraft.Pi.Client.Internal

-- | Send a message to the server.
--
--   Until I find out otherwise, I am going to assume that
--   any conversion done by @show msg@ - such as protecting
--   any double quotes - is sufficient. The spec does not
--   mention what is required.
--
--   There is no attempt to remove or replace invalid characters. The
--   MineCraft server API uses ASCII and it is likely that new lines
--   are not allowed in the message, although this is not specified.
--
chatPost :: String -> MCPI ()
chatPost msg = command "chat.post" [msg]

-- | Return the height of the world at this position. The spec says
--   that the input uses the (x,z) coordinates, but I am assuming this
--   is a typo and am using (x,y) instead.
--
--   It would be easy to test.
getHeight :: IPos -> MCPI Int
getHeight Pos {..} = fromMC `liftM` query "world.getHeight" [toMC _x, toMC _y]
 
