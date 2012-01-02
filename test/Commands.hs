module Commands where

import Data.ByteString       ( ByteString )
import Data.Word             ( Word64 )
import Foreign.Ptr           ( WordPtr )
import System.IO             ( hSetBuffering, BufferMode(LineBuffering), stdin, stdout)

import Network.CCI           ( Status(..) )


data Command =
      ConnectTo String WordPtr (Maybe Word64)
    | Send WordPtr WordPtr ByteString
    | Accept WordPtr
    | Reject WordPtr
    | Disconnect WordPtr
	| WaitEvent
    | Quit
  deriving (Show, Read, Eq)

data Response =
      EndpointCreated String
    | ConnectAccepted WordPtr
    | Recv WordPtr ByteString
    | SendCompletion WordPtr WordPtr Status 
    | Error String
    | ReqAccepted WordPtr
    | ReqRejected WordPtr
    | ReqIgnored WordPtr
    | Rejected WordPtr
    | TimedOut WordPtr
    | KeepAliveTimedOut WordPtr
    | EndpointDeviceFailed
    | Idle
    | Bye
  deriving (Show, Read, Eq)


-- | Initializes communication with the driver process.
initCommands :: IO ()
initCommands = do
  hSetBuffering stdin LineBuffering 
  hSetBuffering stdout LineBuffering 


readCommand :: IO Command
readCommand = fmap read getLine

sendResponse :: Response -> IO ()
sendResponse = print



{-
import Data.Binary           ( Binary(get,put), putWord8, getWord8, runGetState )

instance Binary WordPtr where
    put i = put (fromIntegral i :: Word64)
    get   = fmap (fromIntegral :: Word64 -> WordPtr) get
    
instance Binary Commands.Command where
  put (ConnectTo a b c) = putWord8 0 >> put a >> put b >> put c
  put (Send a b c) = putWord8 1 >> put a >> put b >> put c
  put (Accept a) = putWord8 2 >> put a
  put (Reject a) = putWord8 3 >> put a
  put (Disconnect a) = putWord8 4 >> put a
  put Quit = putWord8 5
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> get >>= \c -> return (ConnectTo a b c)
      1 -> get >>= \a -> get >>= \b -> get >>= \c -> return (Send a b c)
      2 -> get >>= \a -> return (Accept a)
      3 -> get >>= \a -> return (Reject a)
      4 -> get >>= \a -> return (Disconnect a)
      5 -> return Quit
      _ -> fail "no parse"


instance Binary Commands.Response where
  put (ConnectAccepted a) = putWord8 0 >> put a
  put (Recv a b) = putWord8 1 >> put a >> put b
  put (SendCompletion a b c) = putWord8 2 >> put a >> put b >> put c
  put (Error a) = putWord8 3 >> put a
  put (ReqAccepted a) = putWord8 4 >> put a
  put (ReqRejected a) = putWord8 5 >> put a
  put (ReqIgnored a) = putWord8 6 >> put a
  put (Rejected a) = putWord8 7 >> put a
  put (TimedOut a) = putWord8 8 >> put a
  put (KeepAliveTimedOut a) = putWord8 9 >> put a
  put EndpointDeviceFailed = putWord8 10
  put Bye = putWord8 11
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (ConnectAccepted a)
      1 -> get >>= \a -> get >>= \b -> return (Recv a b)
      2 -> get >>= \a -> get >>= \b -> get >>= \c -> return (SendCompletion a b c)
      3 -> get >>= \a -> return (Error a)
      4 -> get >>= \a -> return (ReqAccepted a)
      5 -> get >>= \a -> return (ReqRejected a)
      6 -> get >>= \a -> return (ReqIgnored a)
      7 -> get >>= \a -> return (Rejected a)
      8 -> get >>= \a -> return (TimedOut a)
      9 -> get >>= \a -> return (KeepAliveTimedOut a)
      10 -> return EndpointDeviceFailed
      11 -> return Bye
      _ -> fail "no parse"


instance Binary Network.CCI.Status where
  put SUCCESS = putWord8 0
  put ERROR = putWord8 1
  put ERR_DISCONNECTED = putWord8 2
  put ERR_RNR = putWord8 3
  put ERR_DEVICE_DEAD = putWord8 4
  put ERR_RMA_HANDLE = putWord8 5
  put ERR_RMA_OP = putWord8 6
  put ERR_NOT_IMPLEMENTED = putWord8 7
  put ERR_NOT_FOUND = putWord8 8
  put EINVAL = putWord8 9
  put ETIMEDOUT = putWord8 10
  put ENOMEM = putWord8 11
  put ENODEV = putWord8 12
  put EBUSY = putWord8 13
  put ERANGE = putWord8 14
  put EAGAIN = putWord8 15
  put ENOBUFS = putWord8 16
  put EMSGSIZE = putWord8 17
  put ENOMSG = putWord8 18
  put EADDRNOTAVAIL = putWord8 19
  put (OTHER a) = putWord8 20 >> put a
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> return SUCCESS
      1 -> return ERROR
      2 -> return ERR_DISCONNECTED
      3 -> return ERR_RNR
      4 -> return ERR_DEVICE_DEAD
      5 -> return ERR_RMA_HANDLE
      6 -> return ERR_RMA_OP
      7 -> return ERR_NOT_IMPLEMENTED
      8 -> return ERR_NOT_FOUND
      9 -> return EINVAL
      10 -> return ETIMEDOUT
      11 -> return ENOMEM
      12 -> return ENODEV
      13 -> return EBUSY
      14 -> return ERANGE
      15 -> return EAGAIN
      16 -> return ENOBUFS
      17 -> return EMSGSIZE
      18 -> return ENOMSG
      19 -> return EADDRNOTAVAIL
      20 -> get >>= \a -> return (OTHER a)
      _ -> fail "no parse"
-}


