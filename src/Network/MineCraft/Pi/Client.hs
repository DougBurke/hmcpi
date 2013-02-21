--------------------------------------------------------------------------------
-- |
--  Module      :  Client
--  License     :  Public Domain
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  Haskell 98
--
-- The main interface for connecting to the Raspberry-PI version
-- of MineCraft. The 'Network.MineCraft.Pi.Client.Internal' module
-- provides lower-level access in case this module in insufficient.
--
-- There are two types of calls to MineCraft: "command" and "query".
-- Commands change the state of the server and do not return anything,
-- queries return information from the server, and presumably does not
-- change the server state. This terminology may change.
--
-- I am not sure the use of the @FromMineCraft@ and @ToMineCraft@
-- type classes is justified, given that the API has a very limited
-- set of types.
--
--------------------------------------------------------------------------------

module Network.MineCraft.Pi.Client
    ( MCPI
    , runMCPI

    -- * Conversion routines
    , FromMineCraft(..)
    , ToMineCraft(..)

    -- * Utility routine
    , eRead
    ) where

import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, listToMaybe)

import Network.MineCraft.Pi.Client.Internal

-- | Convert the return value from MineCraft into
--   a Haskell type.
class FromMineCraft a where
  fromMC :: String -> a

-- | Send a value to MineCraft.
class ToMineCraft a where
  toMC :: a -> String

-- Isn't this in base somewhere by now?
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- | Convert a value, but error out if it can not be
--   converted.
eRead :: (Read a) => String -> a
eRead s = fromMaybe (error ("*Conversion error* " ++ s)) $ maybeRead s

instance FromMineCraft Int where
  fromMC = eRead 

instance ToMineCraft Int where
  toMC = show

instance FromMineCraft Float where
  fromMC = eRead

instance ToMineCraft Float where
  toMC = show

instance FromMineCraft a => FromMineCraft [a] where
  fromMC = map fromMC . splitOn ","

instance ToMineCraft a => ToMineCraft [a] where
  toMC = intercalate "," . map toMC

instance (FromMineCraft a, FromMineCraft b) => FromMineCraft (a, b) where
  fromMC s = case splitOn "," s of
                 (a:b:[]) -> (fromMC a, fromMC b)
                 _ -> error $ "*Expected 2 values* " ++ s

instance (ToMineCraft a, ToMineCraft b) => ToMineCraft (a, b) where
  toMC (a,b) = toMC a ++ "," ++ toMC b

