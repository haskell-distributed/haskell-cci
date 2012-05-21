--
-- Copyright (C) 2012 Parallel Scientific. All rights reserved.
--
-- See the accompanying COPYING file for license information.
--

module Commands where

import Data.Word             ( Word64 )
import Foreign.Ptr           ( WordPtr )
import System.IO             ( hSetBuffering, BufferMode(LineBuffering), stdin, stdout, hFlush, hReady)

import Network.CCI           ( Status(..) )


data Command =
      ConnectTo String Int WordPtr (Maybe Word64) -- ^ @ConnectTo uri process_id connection_id timeout@ process id is 
                                                  -- only used on the test driver side
    | Send WordPtr WordPtr Msg                    -- ^ @Send connection_id send_id msg@ message 
    | Accept WordPtr                              -- ^ @Accept connection_id@
    | Reject WordPtr                              -- ^ @Reject connection_id@
    | Disconnect WordPtr                          -- ^ @Disconnect connection_id@
    | WaitConnection WordPtr                      -- ^ Wait for a given connection to be established.
    | WaitSendCompletion WordPtr WordPtr          -- ^ @WaitSendCompletion conn_id send_id@ Wait for a send completion to arrive.
    | WaitRecv WordPtr WordPtr                    -- ^ @WaitRecv conn_id recv_id@ for a receive event to arrive.

    | RMAReuseRMAHandle WordPtr                   -- ^ @RMAReuseRMAHandle conn_id@ marks the connection @conn_id@ to
                                                  -- reuse a previous rma handle instead of creating a new one.
    | RMAHandleExchange WordPtr                   -- ^ @RMAHandleExchange conn_id@ exchanges rma handles with the
                                                  -- given process through the driver.
    | RMAWaitExchange WordPtr                     -- ^ Wait for an rma exchange to complete on the given connection.
    | RMAWrite WordPtr WordPtr                    -- ^ @RMAWrite conn_id msg_id@ 
    | RMARead WordPtr WordPtr                     -- ^ @RMARead conn_id msg_id@ 
    | WaitRMARecv WordPtr WordPtr                 -- ^ @WaitRMARecv conn_id msg_id@ 
    | WaitRMASend WordPtr WordPtr                 -- ^ @WaitRMASend conn_id msg_id@ 

    | Quit
  deriving (Show, Read, Eq)

data Response =
      ConnectAccepted WordPtr                     -- ^ Holds the connection identifier.
    | Recv WordPtr Msg                            -- ^ Holds the connection identifier and the message. 
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

data Msg = Msg WordPtr Int  -- | Message_id and length of the message. See 'msgToString' to learn how the message is generated.
  deriving (Show, Read, Eq)

msgToString :: Msg -> String
msgToString (Msg ctx l) = let n = show ctx in n ++ take (l-length n) (' ' : cycle n)


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

