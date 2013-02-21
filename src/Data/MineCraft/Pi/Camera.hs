--------------------------------------------------------------------------------
-- |
--  Module      :  Camera
--  License     :  Public Domain
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  Haskell 98
--
-- Handle the camera position.
--
-- /Note:/ At present this module does not handle multiple users.
--
--------------------------------------------------------------------------------

module Data.MineCraft.Pi.Camera
    (
      -- * Commands
      setCameraNormal
    , setCameraThirdPerson
    , setCameraFixed
    , setCameraPos
    ) where

import Data.MineCraft.Pi.Types

import Network.MineCraft.Pi.Client
import Network.MineCraft.Pi.Client.Internal

setCameraFixed :: MCPI ()
setCameraFixed = command "camera.mode.setFixed" []

setCameraNormal :: MCPI ()
setCameraNormal = command "camera.mode.setNormal" []

setCameraThirdPerson :: MCPI ()
setCameraThirdPerson = command "camera.mode.setThirdPerson" []

-- | Change the position of the camera.
setCameraPos :: IPos -> MCPI ()
setCameraPos pos = command "camera.mode.setPos" [toMC pos]

-- TODO: the Python API does not have the ThirdPerson variant
--       but it does have setFollow, to follow a user.

