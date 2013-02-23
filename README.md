mcpi
====

All code is placed in the Public Domain.

Author
------

Doug Burke, dburke.gw@gmail.com

Description
-----------

This is a basic interface to the MineCraft-Pi API. The intention is
to build higher-level commands (and add support for missing commands),
but you can say

```haskell
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad (when)

import Data.MineCraft.Pi.Block 
import Data.MineCraft.Pi.Player
import Data.MineCraft.Pi.Types

import Network.MineCraft.Pi.Client

-- | Returns @True@ if the player is standing on gold ore.
isOnGold :: IO Bool
isOnGold = runMCPI $ do
    Pos {..} <- getPlayerTile
    bType <- getBlock $ Pos _x _y (_z-1)
    return (bType == goldOre)

main :: IO ()
main = do
    flag <- isOnGold
    when flag $ putStrLn "Look down!"
```

More examples are included in the 
[examples/](https://github.com/DougBurke/hmcpi/blob/master/examples/)
directory.

