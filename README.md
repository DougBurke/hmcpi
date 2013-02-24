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

Building
--------

 * Install ghc and the Haskell platform

        sudo apt-get install ghc haskell-platform

 * Download the package

        cabal unpack mcpi

   which will create the directory `mcpi-<version>`. If you instead
   say

        cabal install mcpi

   then the package will be built and installed into the system; by
   using `unpack` you just build a local copy.

 * Build the package

        cd mcpi-<version>
        cabal configure
        cabal build

   You may need to install the `pipes` package before the above
   will run:

        cabal install pipes

 * Run one of the examples

        ./dist/build/isongold/isongold 

Examples
--------

Several examples are included in the 
[examples/](https://github.com/DougBurke/hmcpi/blob/master/examples/)
directory. They are built, and installed, by default. To avoid
building them tun off the `build-examples` flag at configure time by
saying

    cabal configure -f-build-examples

