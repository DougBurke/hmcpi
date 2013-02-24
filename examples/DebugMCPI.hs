{-
License:

This code is placed in the Public Domain.

Author:

Douglas Burke (dburke.gw@gmail.com)

Usage:

  ./debugmcpi

Similar to MCPI.hs except that the user input is converted into
a library call. The \"command language\" is

  command name [arg1 .. argn]
  query name [arg1 .. argn]
  quit
  exit

and is case sensitive.

This is to support debugging the interface.

-}

module Main where

import Control.Monad (void)
import Control.Monad.IO.Class

import Data.Char (isSpace)

import Network.MineCraft.Pi.Client.Internal (MCPI, runMCPI', command, query)

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

import Utils (printVersion)

-- | Remove leading and trailing whitespace.
strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
  
-- | Ask the user for a command, execute it and, if necessary, 
--   wait for a response.
--
userInteract :: MCPI () 
userInteract = do
  user <- liftIO getLine
  handleUser (strip user)
  userInteract
  
-- | For queries I am currently relying on the debug output of
--   @query@ rather than handle the output here.
handleUser :: String -> MCPI ()
handleUser [] = return ()
handleUser "quit" = liftIO $ exitSuccess
handleUser "exit" = liftIO $ exitSuccess
handleUser user = 
  case words user of
    ("command":comm:args) -> command comm args
    ("query":qry:args) -> void (query qry args)
    _ -> liftIO $ putStrLn "Expected quit, exit, command ..., or query ..."
    
usage :: IO ()
usage = do
    progName <- getProgName
    hPutStrLn stderr $ "Usage: " ++ progName
    exitFailure

main :: IO ()
main = do
  args <- getArgs
  if null args
    then printVersion >> runMCPI' True userInteract
    else usage

