{-
License:

This code is placed in the Public Domain.

Author:

Douglas Burke (dburke.gw@gmail.com)

Usage:

  ./hmcpi

Connects to MineCraft-PI and allows you to enter commands (it can be
thought of as telnet specialized to connect to just MineCraft).

At present there is no attempt to provide an improved interface, such
as help messages, auto-completion of commands, ignoring blank or
comment lines. These could be added, but at least the current
implementation does let you see exactly what is sent to and from
MineCraft-PI.

One useful change would be for control-D - or some other terminal
character or string - to exit the program. At the moment the only
way to exit is to kill the program (e.g. with control-C).

-}

module Main where

import qualified Control.Exception as CE

import Control.Concurrent (forkIO)
import Control.Monad (when)

import Network (PortID (..), connectTo)

import Pipes
import qualified Pipes.Prelude as P

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO ( BufferMode(LineBuffering), Handle
                 , hClose, hIsTerminalDevice
                 , hPutStrLn, hSetBuffering
                 , stderr, stdin, stdout )

import Utils (printVersion)

-- | Link together the two handles, so that content
--   moves from the first to the second.
--
--   It might be nice to clean out user input - e.g. skip blank
--   or empty lines - as well as provide support (e.g. help), but
--   for now just pass through everything. This at least means
--   that we can see what the server is actually doing, and
--   means we can use the same code for connecting user input
--   to the server as well as displaying the user output.
--
pipeTo :: Handle -> Handle -> IO ()
pipeTo inHdl outHdl =
    runEffect $ P.fromHandle inHdl >-> P.toHandle outHdl

{- How best to handle a user exit? Need to signal to the
   main program that we have finished

pipeToWithExit :: Handle -> Handle -> IO ()
pipeToWithExit inHdl outHdl =
    runEffect $ P.fromHandle inHdl >->
                P.takeWhile ('\EOT' `notElem`) >->
                P.toHandle outHdl

-}

pipeToWithExit :: Handle -> Handle -> IO ()
pipeToWithExit = pipeTo


-- | Only display the message if @stdin@ is connected to
--   a terminal device (i.e. the input is not being piped
--   from a file).
--
logMsg :: String -> IO ()
logMsg msg = hIsTerminalDevice stdin >>= \flag -> when flag (putStrLn msg)

mcpi :: IO ()
mcpi = 
    let errorHandler :: CE.IOException -> IO ()
        errorHandler _ = do
          hPutStrLn stderr "ERROR: Unable to connect MineCraft. Is it running?"
          exitFailure

    in CE.bracket
           (connectTo "127.0.0.1" (PortNumber 4711))
           hClose
           (\server -> do
              logMsg "*** Connected to MineCraft"
              mapM_ (`hSetBuffering` LineBuffering) [stdin, stdout, server]
              _ <- forkIO $ stdin `pipeToWithExit` server
              server `pipeTo` stdout
           )
           `CE.catch` errorHandler
               
usage :: IO ()
usage = do
    progName <- getProgName
    hPutStrLn stderr $ "Usage: " ++ progName
    exitFailure

main :: IO ()
main = do
  args <- getArgs
  if null args
    then printVersion >> mcpi
    else usage

