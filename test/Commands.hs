--
-- Copyright (C) 2012 Parallel Scientific. All rights reserved.
--
-- See the accompanying COPYING file for license information.
--

module Commands where

import Data.Word             ( Word64 )
import Data.ByteString       ( ByteString )
import qualified Data.ByteString.Char8 as B8 ( unpack )
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
	| RMAPrepareRead WordPtr WordPtr              -- ^ @RMAPrepareRead conn_id msg_id@ writes the rma buffer with predefined data for an rma operation.
    | RMARead WordPtr WordPtr                     -- ^ @RMARead conn_id msg_id@ 
	| RMAWaitWrite WordPtr WordPtr                -- ^ @RMAWaitWrite conn_id msg_id@
	| RMAWaitRead WordPtr WordPtr                 -- ^ @RMAWaitRead conn_id msg_id@
	| RMAFreeHandles WordPtr                      -- ^ @RMAFreeHandles conn_id@ releases the handles of a connection.

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
         | MsgRMAH ByteString -- | Carries an rma handle 
		 | MsgRMARead WordPtr -- | Carries an rmaRead identifier.
		 | MsgRMAWrite WordPtr -- | Carries an rmaWrite identifier.
  deriving (Show, Read, Eq)

msgToString :: Msg -> String
msgToString (Msg ctx l) = let n = show ctx in n ++ take (l-length n) (' ' : cycle n)
msgToString (MsgRMAH bs) = "rmaH "++B8.unpack bs
msgToString (MsgRMARead ctx) = "rmaRead "++show ctx
msgToString (MsgRMAWrite ctx) = "rmaWrite "++show ctx


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

