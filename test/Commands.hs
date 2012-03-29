--
-- Copyright (C) 2012 Parallel Scientific. All rights reserved.
--
-- See the accompanying COPYING file for license information.
--

module Commands where

import Data.ByteString       ( ByteString )
import Data.Word             ( Word64 )
import Foreign.Ptr           ( WordPtr )
import System.IO             ( hSetBuffering, BufferMode(LineBuffering), stdin, stdout, hFlush, hReady)

import Network.CCI           ( Status(..) )


data Command =
      ConnectTo String Int WordPtr (Maybe Word64) -- ^ @ConnectTo uri process_id connection_id timeout@ process id is 
                                                  -- only used on the test driver side
    | Send WordPtr WordPtr ByteString             -- ^ @Send connection_id send_id "send_id"@ message 
    | Accept WordPtr                              -- ^ @Accept connection_id@
    | Reject WordPtr                              -- ^ @Reject connection_id@
    | Disconnect WordPtr                          -- ^ @Disconnect connection_id@
    | WaitConnection WordPtr                      -- ^ Wait for a given connection to be established.
    | WaitSendCompletion WordPtr WordPtr          -- ^ @WaitSendCompletion conn_id send_id@ Wait for a send completion to arrive.
    | WaitRecv WordPtr WordPtr                    -- ^ @WaitRecv conn_id recv_id@ for a receive event to arrive.
    | Quit
  deriving (Show, Read, Eq)

data Response =
      ConnectAccepted WordPtr                     -- ^ Holds the connection identifier.
    | Recv WordPtr ByteString                     -- ^ Holds the connection identifier and the message. 
    | SendCompletion WordPtr WordPtr Status       -- ^ Holds the connection identifier the send identifier and the status.
    | Error String
    | ReqAccepted WordPtr                         -- ^ Holds the connection identifier.
    | ReqRejected WordPtr                         -- ^ Holds the connection identifier. 
    | ReqIgnored WordPtr                          -- ^ Holds the connection identifier.
    | Rejected WordPtr                            -- ^ Holds the connection identifier. This response is given when the remote
                                                  -- side rejects a connection request.
    | TimedOut WordPtr                            -- ^ Holds the connection identifier of the connect request that timed out.
    | KeepAliveTimedOut WordPtr                   -- ^ Hodls the connection identifier.
    | EndpointDeviceFailed
    | Idle
  deriving (Show, Read, Eq)


-- | Initializes communication with the driver process.
initCommands :: IO ()
initCommands = do
  hSetBuffering stdin LineBuffering 
  hSetBuffering stdout LineBuffering 


readCommand :: IO Command
readCommand = fmap read getLine

tryReadCommand :: IO (Maybe Command)
tryReadCommand = do
  isReady <- hReady stdin
  if isReady then fmap (Just . read) getLine
    else return Nothing


sendResponse :: Response -> IO ()
sendResponse r = print r >> hFlush stdout

