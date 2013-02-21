{-# LANGUAGE RecordWildCards #-}

--------------------------------------------------------------------------------
-- |
--  Module      :  Internal
--  License     :  Public Domain
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  RecordWildCards
--
-- Internal types for connecting to the Raspberry-PI version
-- of MineCraft. Most users are expected to use "Network.MineCraft.Pi.Client"
-- rather than this module, but it is provided in case the former
-- is not sufficient.
--
--------------------------------------------------------------------------------

module Network.MineCraft.Pi.Client.Internal
    ( MCPI
    , runMCPI
    , runMCPI'
    , query
    , command
    ) where

import qualified Control.Exception as CE

import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Data.List (intercalate)

import Network.BSD
import Network.Socket

import System.Exit
import System.IO

-- | Represent a program that communicates with a MineCraft PI
--   server.
--
type MCPI = ReaderT ConnInfo IO

-- | Connection information.
data ConnInfo = ConnInfo {
      _ciHandle :: Handle  -- ^ Connection to the MineCraft program
    , _ciDebug :: Bool     -- ^ Should messages to and from MineCraft
                           --   be printed to @stderr@.
    }

-- | Commands do not return anything, queries do.
type Command = String
type Query = String
type Argument = String

-- | The port used by MineCraft is fixed.
mcPort :: String
mcPort = "4711"

-- | Open a connection to the Minecraft server or call
--   exitFailure, after displaying an error message to @stderr@.
--
openMCPI ::
    Bool  -- ^ Set to @True@ to get debugging messages printed to @stderr@.
    -> IO ConnInfo
openMCPI flag = do
    let ehdl :: CE.IOException -> IO ()
        ehdl _ = hPutStrLn stderr "ERROR: Unable to connect to MineCraft-PI. Is it running?" >>
                 exitFailure
       
    as <- getAddrInfo Nothing Nothing (Just mcPort)
    let a = head as -- note: getAddrInfo never returns an empty list
    sock <- socket (addrFamily a) Stream defaultProtocol
    setSocketOption sock KeepAlive 1
    connect sock (addrAddress a) `CE.catch` ehdl
    h <- socketToHandle sock ReadWriteMode
    hSetBuffering h LineBuffering

    return $ ConnInfo h flag

-- | Close the connection.
closeMCPI :: ConnInfo -> IO ()
closeMCPI = hClose . _ciHandle

logMsg :: ConnInfo -> String -> String -> MCPI ()
logMsg ConnInfo {..} hdr msg = 
  when _ciDebug $ liftIO $ hPutStrLn stderr $ "*DBG*" ++ hdr ++ "*" ++ msg

-- It would be nice to do the argument marshalling here, i.e. have
-- something like @command :: Command -> [Argument] -> MCPI ()@
-- which would be run like @command "player.setTile" [Pos 0 0 0]@,
-- but I do not want to deal with heterogeneous lists at this time.
-- Instead, we force the caller to do the conversion.
--
addArgs :: String -> [Argument] -> String
addArgs a bs = a ++ "(" ++ intercalate "," bs ++ ")"

-- | Run a MineCraft command. 
command :: Command -> [Argument] -> MCPI ()
command comm args = do
  ci <- ask
  let commstr = addArgs comm args
  logMsg ci "COMMAND" commstr
  liftIO $ hPutStrLn (_ciHandle ci) commstr

-- | Run a MineCraft query, returning the response.
query :: Query -> [Argument] -> MCPI String
query qry args = do
  ci <- ask
  let qrystr = addArgs qry args
  logMsg ci "QUERY" qrystr
  liftIO $ hPutStrLn (_ciHandle ci) qrystr
  ans <- liftIO $ hGetLine (_ciHandle ci)
  logMsg ci "RESPONSE" ans
  return ans

-- | Run a Raspberry-PI program. The flag determines whether the
--   messages sent to, and received from, the server, are
--   printed to @stderr@.
--
--   An exception is raised if the server is not running, or
--   can not be contacted.
runMCPI' :: Bool -> MCPI a -> IO a
runMCPI' flag p = 
  bracket
    (openMCPI flag)
    closeMCPI
    (runReaderT p)

-- | Run a Raspberry-PI program.
--
--   An exception is raised if the server is not running, or
--   can not be contacted.
runMCPI :: MCPI a -> IO a
runMCPI = runMCPI' False

