--
-- Copyright (C) 2012 Parallel Scientific. All rights reserved.
--
-- See the accompanying COPYING file for license information.
--


import Control.Monad         ( forever, void, forM_, replicateM_, when )
import qualified Data.ByteString as B  ( empty, take, length, head, singleton, null )
import Data.ByteString.Char8 ( pack, unpack )
import Data.ByteString.Unsafe ( unsafePackCStringLen )
import Data.Maybe            ( isNothing, fromJust )
import Data.Time             ( getCurrentTime, diffUTCTime )
import Data.Word             ( Word64 )
import Foreign.Storable      ( poke )
import Foreign.Marshal.Alloc ( allocaBytesAligned )
import Network.CCI           ( withCCI, withEndpoint, endpointURI, accept, Endpoint
                             , pollWithEventData, EventData(..), send, Connection
                             , connect, ConnectionAttributes(..), connectionMaxSendSize
                             , createRMARemoteHandle, RMARemoteHandle, rmaWrite
                             , withRMALocalHandle, rmaHandle2ByteString, RMA_MODE(..)
                             , unsafePackEventBytes, packEventBytes, disconnect
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
  , oIters = 16*1024
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
    if null nonOptions && null errors && isNothing (oServerURI o) == oIsServer o then
      withCCI$
        withEndpoint Nothing$ \(ep,_fd) -> do
          endpointURI ep >>= putStrLn
          if oIsServer o then forever$ goServer ep o
            else goClient ep o
     else
      putStrLn$ unlines$ errors ++ [ usageInfo header options ]
    putStrLn "type something"
    void$ getLine
    putStrLn "type something else"
  where 
    header = "Usage: pingpong [OPTION...]"


goServer :: Endpoint -> Options -> IO ()
goServer ep _o = do
    pollWithEventData ep$ \evd ->
        case evd of
          EvConnectRequest ev _bs _cattr  -> void$ accept ev 0
          _ -> print evd

    pollWithEventData ep$ \evd ->
        case evd of
          EvAccept 0 (Right _conn)  -> return ()
          _ -> print evd

    (conn,cs) <- pollWithEventData ep$ \evd ->
        case evd of
          EvRecv ebs conn -> packEventBytes ebs >>= return . (,) conn . read . unpack
          _ -> error ("goServer: unexpected event"++show evd)
    case cs of
      AM     -> connectionMaxSendSize conn >>= goServer'
      RMA sz -> do 
        allocaBytesAligned (fromIntegral sz) 4096 $ \cbuf ->
          withRMALocalHandle ep (cbuf,fromIntegral sz) RMA_READ_WRITE$ \lh -> do
            send conn (rmaHandle2ByteString lh) 0 []
            goServer' sz
  where
    goServer' sz = allocaBytesAligned (fromIntegral sz) 4096 $ \cbuf ->
      unsafePackCStringLen (cbuf,fromIntegral sz) >>= \sbuf -> do
        Just conn <- loopWhileM isNothing$ pollWithEventData ep$ \evd ->
          case evd of
            EvRecv ebs conn -> do
                bs <- unsafePackEventBytes ebs
                if not (B.null bs) && B.head bs == 1 then
                  return$ Just conn
                 else
                  send conn (B.take (B.length bs) sbuf) 0 [] >> return Nothing
            EvSend _ _ _    -> return Nothing
            _ -> print evd >> return Nothing

        disconnect conn


goClient :: Endpoint -> Options -> IO ()
goClient ep o = do
    connect ep (fromJust$ oServerURI o) B.empty CONN_ATTR_RO 0 Nothing
    conn <- pollWithEventData ep$ \evd ->
        case evd of
          EvConnect _ctx (Right conn) -> return conn
          _                           -> print evd >> return (error "goClient: unexpected event.")
    mrmah <- exchangeConnectionSpecs ep o conn
    sz <- if isNothing mrmah then fmap fromIntegral$ connectionMaxSendSize conn
            else return$ fromIntegral$ fromJust$ oRMA o
    allocaBytesAligned sz 4096 $ \cbuf -> do
      when (sz>0)$ poke cbuf 0
      unsafePackCStringLen (cbuf,sz) >>= \sbuf ->
        case mrmah of
          Nothing -> goClient' conn sbuf Nothing
          Just rh -> withRMALocalHandle ep (cbuf,sz) RMA_READ_WRITE (goClient' conn sbuf . Just . flip (,) rh)
  where
    goClient' conn sbuf mrmahs = do
      putStrLn "Bytes\t\tLatency (one-way)\tThroughput"
      let app = if isNothing mrmahs then (0:) else id
          bszs = takeWhile (<=B.length sbuf)$ app$ iterate (*2) 1
          its = replicate (length$ takeWhile (<1024*(64::Int)) bszs) 0 ++ [0::Int ..]
      forM_ (zip bszs its)$ \(current_size,i) -> do

        let warmup = max 2$ oWarmup o `div` 2^i
        let iters = max 16$ oIters o `div` 2^i

        replicateM_ warmup$ testRoundTrip conn sbuf mrmahs current_size

        start <- getCurrentTime
        replicateM_ iters$ testRoundTrip conn sbuf mrmahs current_size
        end <- getCurrentTime

        let lat = toRational (diffUTCTime end start) * 1000000 / toRational iters
                    / if isNothing mrmahs then 2 else 1
            bw = fromIntegral current_size / lat;
        printf "%8d\t%8.2f us\t\t%8.2f MB/s\n" current_size (fromRat lat :: Double) (fromRat bw :: Double)

      -- say goodbye
      send conn (B.singleton 1) 0 []
      void$ loopWhileM id$ pollWithEventData ep$ \evd ->
        case evd of
          EvSend _ _ _    -> return False
          _               -> error ("goClient': unexpected event: "++show evd)
      disconnect conn


    testRoundTrip conn _sbuf (Just (lh,rh)) msg_size = do
        rmaWrite conn Nothing rh 0 lh 0 (fromIntegral msg_size) 0 []
        void$ loopWhileM id$ pollWithEventData ep$ \evd ->
          case evd of
            EvSend _ _ _ -> return False
            _            -> print evd >> return True

    testRoundTrip conn sbuff Nothing msg_size = do
        send conn (B.take (fromIntegral msg_size) sbuff) 0 []
        void$ loopWhileM id$ pollWithEventData ep$ \evd ->
          case evd of
            EvRecv _ _conn  -> return False
            EvSend _ _ _    -> return True
            _               -> print evd >> return True

exchangeConnectionSpecs :: Endpoint -> Options -> Connection -> IO (Maybe RMARemoteHandle)
exchangeConnectionSpecs ep o conn = do
    let cs = connectionSpecs o
    send conn (pack$ show cs) 0 []
    case cs of
      RMA _ -> loopWhileM isNothing$ pollWithEventData ep$ \evd ->
        case evd of
            EvRecv ebs _conn -> packEventBytes ebs >>= return . createRMARemoteHandle
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

