

import Control.Monad         ( forever, void, forM_, replicateM_ )
import Data.ByteString       ( ByteString )
import qualified Data.ByteString as B  ( empty, take, length )
import Data.ByteString.Char8 ( pack, unpack )
import Data.ByteString.Unsafe ( unsafePackCStringLen )
import Data.Maybe            ( isNothing, fromJust )
import Data.Time             ( getCurrentTime, diffUTCTime )
import Data.Word             ( Word64 )
import Foreign.Marshal.Alloc ( allocaBytesAligned )
import Network.CCI           ( initCCI, withEndpoint, endpointURI, accept, Endpoint
                             , pollWithEventData, EventData(..), send, Connection
                             , connect, ConnectionAttributes(..), connectionMaxSendSize
                             , createRMARemoteHandle, RMARemoteHandle, rmaWrite
                             , withConnectionRMALocalHandle, rmaHandle2ByteString
                             , VolatileByteString(..)
                             )
import Numeric               ( fromRat )
import Text.Printf           ( printf )
import System.Console.GetOpt ( OptDescr(..), ArgDescr(..), getOpt, ArgOrder(..), usageInfo )
import System.Environment    ( getArgs )

data Options = Options
   { oServerURI :: Maybe String
   , oIsServer :: Bool
   , oWarmup :: Int
   , oIters :: Int
   , oRMA :: Maybe Word64
   }

defaultOptions :: Options
defaultOptions = Options
  { oServerURI = Nothing
  , oIsServer = False
  , oWarmup = 1024
  , oIters = 512*1024
  , oRMA = Nothing
  }

data ConnectionSpecs = AM | RMA Word64
  deriving (Show, Read)

options :: [ OptDescr (Options -> Options) ]
options = 
    [ Option "h" [] (ReqArg (\uri o -> o { oServerURI = Just uri }) "URI") "server URI" 
    , Option "s" [] (NoArg (\o -> o { oIsServer = True })) "run as server"
    , Option "r" [] (ReqArg (\sz o -> o { oRMA = Just$ read sz }) "BUFSIZE") "use rma with the given buffer size"
    ]

main :: IO ()
main = do
    args <- getArgs
    let (fns,nonOptions,errors) = getOpt Permute options args
        o = foldl (flip ($)) defaultOptions fns
    if null nonOptions && null errors && isNothing (oServerURI o) == oIsServer o then do
      initCCI
      withEndpoint Nothing$ \(ep,_fd) -> do
        endpointURI ep >>= putStrLn
        if oIsServer o then goServer ep o
          else goClient ep o
     else
      putStrLn$ unlines$ errors ++ [ usageInfo header options ]
  where 
    header = "Usage: pingpong [OPTION...]"


goServer :: Endpoint -> Options -> IO ()
goServer ep _o = do
    pollWithEventData ep$ \evd ->
        case evd :: EventData ByteString of
          EvConnectRequest ev _bs _cattr  -> void$ accept ev
          _ -> print evd

    (conn,cs) <- pollWithEventData ep$ \evd ->
        case evd of
          EvRecv bs conn -> return$ (,) conn$ read$ unpack bs
          _ -> print evd >> return undefined
    case cs of
      AM     -> connectionMaxSendSize conn >>= goServer'
      RMA sz -> do 
        allocaBytesAligned (fromIntegral sz) 4096 $ \cbuf ->
          withConnectionRMALocalHandle conn (cbuf,fromIntegral sz)$ \lh -> do
            send conn (rmaHandle2ByteString lh) 0 []
            goServer' sz
  where
    goServer' sz = allocaBytesAligned (fromIntegral sz) 4096 $ \cbuf ->
      unsafePackCStringLen (cbuf,fromIntegral sz) >>= \sbuf ->
        forever$ pollWithEventData ep$ \evd ->
          case evd of
            EvRecv (VolatileB bs) conn       -> send conn (B.take (B.length bs) sbuf) 0 []
            EvSend _ _ _                    -> return ()
            _ -> print evd
      

goClient :: Endpoint -> Options -> IO ()
goClient ep o = do
    connect ep (fromJust$ oServerURI o) B.empty CONN_ATTR_RO 0 Nothing
    conn <- pollWithEventData ep$ \evd ->
        case evd :: EventData ByteString of
          EvConnectAccepted _ctx conn -> return conn
          _                           -> print evd >> return undefined
    mrmah <- exchangeConnectionSpecs ep o conn
    sz <- if isNothing mrmah then fmap fromIntegral$ connectionMaxSendSize conn
            else return$ fromIntegral$ fromJust$ oRMA o
    allocaBytesAligned sz 4096 $ \cbuf ->
      unsafePackCStringLen (cbuf,sz) >>= \sbuf ->
        case mrmah of
          Nothing -> goClient' conn sbuf Nothing
          Just rh -> withConnectionRMALocalHandle conn (cbuf,sz) (goClient' conn sbuf . Just . flip (,) rh)
  where
    goClient' conn sbuf mrmahs = do
      putStrLn "Bytes\t\tLatency (one-way)\tThroughput"
      let app = if isNothing mrmahs then (0:) else id
      forM_ (takeWhile (<=B.length sbuf)$ app$ iterate (*2) 1)$ \current_size -> do

        replicateM_ (oWarmup o)$ testRoundTrip conn sbuf mrmahs current_size

        start <- getCurrentTime
        replicateM_ (oIters o)$ testRoundTrip conn sbuf mrmahs current_size
        end <- getCurrentTime

        let lat = toRational (diffUTCTime end start) * 1000000 / toRational (oIters o) 
                    / if isNothing mrmahs then 2 else 1
        -- let lat = toRational (diffUTCTime end start) * 1000000 / 2 / toRational (oIters o)
            bw = fromIntegral current_size / lat;
        printf "%8d\t%8.2f us\t\t%8.2f MB/s\n" current_size (fromRat lat :: Double) (fromRat bw :: Double)

    testRoundTrip conn _sbuf (Just (lh,rh)) msg_size = do
        rmaWrite conn Nothing rh 0 lh 0 (fromIntegral msg_size) 0 []
        void$ loopWhileM id$ pollWithEventData ep$ \evd ->
          case evd :: EventData VolatileByteString of
            EvSend _ _ _ -> return False
            _               -> print evd >> return True
    testRoundTrip conn sbuff Nothing msg_size = do
        send conn (B.take (fromIntegral msg_size) sbuff) 0 []
        void$ loopWhileM id$ pollWithEventData ep$ \evd ->
          case evd of
            EvRecv bs _conn -> let _ = bs :: VolatileByteString in return False
            EvSend _ _ _    -> return True
            _               -> print evd >> return True

exchangeConnectionSpecs :: Endpoint -> Options -> Connection -> IO (Maybe RMARemoteHandle)
exchangeConnectionSpecs ep o conn = do
    let cs = connectionSpecs o
    send conn (pack$ show cs) 0 []
    case cs of
      RMA _ -> loopWhileM isNothing$ pollWithEventData ep$ \evd ->
        case evd of
            EvRecv bs _conn -> return$ createRMARemoteHandle bs
            EvSend _ _ _    -> return Nothing
            _               -> print evd >> return Nothing
      _ -> return Nothing

connectionSpecs :: Options -> ConnectionSpecs
connectionSpecs = maybe AM RMA . oRMA


-- foreign import ccall unsafe posix_memalign :: Ptr (Ptr ()) -> Word32 -> Word32 -> IO Int


-- | @loopWhileM p io@ performs @io@ repeteadly while its result satisfies @p@.
-- Yields the first offending result.
loopWhileM :: Monad m => (a -> Bool) -> m a -> m a
loopWhileM p m = go
  where go = m >>= \a -> if p a then go else return a

-- | @foreverSt s f@ performs @f@ repeteadly which modifies a state initialized with @s@.
-- Never finishes unless there is an exception.
-- foreverSt :: Monad m => a -> (a -> m a) -> m b 
-- foreverSt a fm = fm a >>= \a' -> foreverSt a' fm

