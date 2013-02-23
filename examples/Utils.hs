{-
License:

This code is placed in the Public Domain.

Author:

Douglas Burke (dburke.gw@gmail.com)

Aim:

Utility routines for the examples.

-}

module Utils 
       ( checkForDebug
       , maybeRead
       , printVersion
       ) where

import Data.List (foldl')
import Data.Maybe (listToMaybe)
import Data.Version (showVersion)

import System.Environment (getProgName)

import Paths_mcpi (version)

-- | Print, to @stdout@, the program name and version.
printVersion :: IO ()
printVersion = do
  pName <- getProgName
  putStrLn (pName ++ " - version " ++ showVersion version)
  
-- | Does the argument list contain the option @--debug@? 
--   If so, remove it.
--
checkForDebug :: [String] -> (Bool, [String])
checkForDebug inArgs =
    let f (flag,args) arg
            | arg == "--debug" = (True, args)
            | otherwise        = (flag, arg:args)
    in foldl' f (False, []) (reverse inArgs)

-- | Convert a string, with the option of failure.
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

