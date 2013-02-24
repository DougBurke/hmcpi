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

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (unless, when)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Data.List (intercalate)

import Foreign
import Foreign.C.String

import Network.BSD
import Network.Socket

import System.Exit
import System.IO

-- | Represent a program that communicates with a MineCraft PI
--   server.
--
--   /TODO:/ run a computation without automatically opening
--         and closing the handle.
--
type MCPI = ReaderT ConnInfo IO

-- | Connection information.
--
-- /TODO:/ Should the buffer used by @flushChannel@ be stored here, to
--         avoid repeated allocation/de-allocation?  It is unlikely (I
--         speculate) to be a major optimisation in time or space.
--
data ConnInfo = ConnInfo {
      _ciHandle :: Handle  -- ^ Connection to the MineCraft program
    , _ciDebug :: Bool     -- ^ Should messages to and from MineCraft
                           --   be printed to @stderr@.
    , _ciDelay :: Int      -- ^ Delay, in microseconds, to wait after
                           --   making a call with `command` before
                           --   returning. *Experimental*
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
--   /TODO:/ Change the retun value to @Maybe ConnInfo@ and
--           make this public.
openMCPI ::
  Int      -- ^ Delay, in microseconds, to wait after making a `command`.
  -> Bool  -- ^ Set to @True@ to get debugging messages printed to @stderr@.
  -> IO ConnInfo
openMCPI delay flag = do
    let ehdl :: CE.IOException -> IO ()
        ehdl _ = 
           hPutStrLn stderr "ERROR: Unable to connect to MineCraft-PI. Is it running?" >>
           exitFailure
       
    as <- getAddrInfo Nothing Nothing (Just mcPort)
    let a = head as -- note: getAddrInfo never returns an empty list
    sock <- socket (addrFamily a) Stream defaultProtocol
    setSocketOption sock KeepAlive 1
    setSocketOption sock NoDelay 1
    connect sock (addrAddress a) `CE.catch` ehdl
    h <- socketToHandle sock ReadWriteMode
    hSetBuffering h LineBuffering

    return $ ConnInfo h flag delay

-- | Close the connection.
--
--   /TODO:/ Make this public.
closeMCPI :: ConnInfo -> IO ()
closeMCPI = hClose . _ciHandle

-- | Write a debug message to @stderr@ if the debug flag is set. The
--   format is \"[type] msg\".
--
logMsg ::
  ConnInfo
  -> String  -- ^ The type of message (ideally 8 characters or less to
             --   keep the output aligned, but can be larger). Trailing
             --   white space is ignored.
  -> String  -- ^ The message to display.
  -> IO ()
logMsg ConnInfo {..} hdr msg = 
  when _ciDebug $ hPutStrLn stderr 
                $ "[" ++ hdr ++ replicate (8 - length hdr) ' ' ++
                  "] " ++ reverse (dropWhile isSpace (reverse msg))

-- | Add arguments to a query or command to create the
--   string to send to MineCraft.
addArgs :: String -> [Argument] -> String
addArgs a bs = a ++ "(" ++ intercalate "," bs ++ ")"

-- | At present the only "error" message I have seen is "Fail\0",
--   so we use a small buffer size.
bufSize :: Int
bufSize = 16

-- | Return from MineCraft that indicates an error.
connectionError :: String
connectionError = "Fail"

-- | Remove any output from the handle. A debug message is written
--   to let the user know this has happened, presumably because of
--   a previous invalid call. The previous call could be stored in
--   ConnInfo to make the message more useful, but leave that for now.
--
flushChannel :: ConnInfo -> IO ()
flushChannel ci@ConnInfo {..} = do
  -- Should the buffer be stored in ConnInfo so we don't have
  -- to repeatedly allocate it? It's only small (at present),
  -- so probably not an issue.
  fbuf <- mallocForeignPtrBytes bufSize 
  withForeignPtr fbuf $ \bufPtr -> 
    let loop store = do
          nrec <- hGetBufNonBlocking _ciHandle bufPtr bufSize
          if (nrec > 0)
            then peekCStringLen (bufPtr, nrec) >>= \str -> loop (store ++ str)
            else unless (null store) $ logMsg ci "FLUSH" store
    in loop []

-- | Run a MineCraft command. There is a pause after making the command,
--   which is an attempt to allow any invalid commands to return a
--   message, so that it can be handled by the next call of `command`
--   or `query`. This approach is *experimental* and may change.
command :: Command -> [Argument] -> MCPI ()
command comm args = do
  ci <- ask
  let commstr = addArgs comm args
  liftIO $ 
    flushChannel ci
    >> logMsg ci "COMMAND" commstr
    >> hPutStrLn (_ciHandle ci) commstr
    >> threadDelay (_ciDelay ci)

-- | Run a MineCraft query, returning the response. An
--   @IOError@ is raised if the response is @Fail@ (this
--   might be due to a previous command failing, depending
--   on the time taken by MineCraft to respond).
--
query :: Query -> [Argument] -> MCPI String
query qry args = do
  ci <- ask
  let qryStr = addArgs qry args
  liftIO $ 
    flushChannel ci
    >> logMsg ci "QUERY" qryStr
    >> hPutStrLn (_ciHandle ci) qryStr
  ans <- liftIO $ hGetLine (_ciHandle ci)
  liftIO $ logMsg ci "RESPONSE" ans
  when (ans == connectionError) $
    liftIO $ ioError $ userError $ "Query failed: " ++ qryStr
  return ans

-- | Try a 0.1 second delay.
testDelay :: Int
testDelay = 100000

-- | Run a Raspberry-PI program. The flag determines whether the
--   messages sent to, and received from, the server, are
--   printed to @stderr@ as a diagnostic.
--
--   An exception is raised if the server is not running, or
--   can not be contacted.
runMCPI' :: Bool -> MCPI a -> IO a
runMCPI' flag p = 
  bracket
    (openMCPI testDelay flag)
    closeMCPI
    (runReaderT p)

-- | Run a Raspberry-PI program.
--
--   An exception is raised if the server is not running, or
--   can not be contacted.
runMCPI :: MCPI a -> IO a
runMCPI = runMCPI' False

