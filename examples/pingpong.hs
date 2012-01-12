

import Control.Monad         ( forever, void, forM_, replicateM_ )
import Data.ByteString       ( ByteString )
import qualified Data.ByteString as B  ( replicate, empty, take )
import Data.Maybe            ( isNothing, fromJust )
import Data.Time             ( getCurrentTime, diffUTCTime )
import Network.CCI           ( initCCI, withEndpoint, endpointURI, accept, Endpoint
                             , pollWithEventData, EventData(..), send
                             , connect, ConnectionAttributes(..), connectionMaxSendSize
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
   }

defaultOptions :: Options
defaultOptions = Options
  { oServerURI = Nothing
  , oIsServer = False
  , oWarmup = 1024
  , oIters = 512*1024
  }

options :: [ OptDescr (Options -> Options) ]
options = 
    [ Option "h" [] (ReqArg (\uri o -> o { oServerURI = Just uri }) "URI") "server URI" 
    , Option "s" [] (NoArg (\o -> o { oIsServer = True })) "run as server"
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
goServer ep _o = 
    void$ forever$ pollWithEventData ep$ \evd ->
        case evd of
          EvConnectRequest ev _bs _cattr  -> void$ accept ev
          EvRecv bs conn                  -> send conn bs 0 []
          EvSend _ _ _                    -> return ()
          _ -> print evd
      

goClient :: Endpoint -> Options -> IO ()
goClient ep o = do
    connect ep (fromJust$ oServerURI o) B.empty CONN_ATTR_UU 0 Nothing
    loopWhileM isNothing$ pollWithEventData ep$ \evd ->
        case evd :: EventData ByteString of
          EvConnectAccepted _ctx conn -> return$ Just conn
          _                           -> print evd >> return Nothing
    >>= maybe (return ()) goClient'
  where
    goClient' conn = do
      putStrLn "Bytes\t\tLatency (one-way)\tThroughput"
      mxSize <- connectionMaxSendSize conn
      let sbuff = B.replicate (fromIntegral mxSize) (toEnum$ fromEnum 'b')
      forM_ (takeWhile (<=mxSize)$ 0:iterate (*2) 1)$ \current_size -> do

        replicateM_ (oWarmup o)$ testRoundTrip conn sbuff current_size

        start <- getCurrentTime
        replicateM_ (oIters o - 1)$ testRoundTrip conn sbuff current_size
        end <- getCurrentTime

        let lat = toRational (diffUTCTime end start) * 1000000 / 2 / toRational (oIters o)
            bw = fromIntegral current_size / lat;
        printf "%8d\t%8.2f us\t\t%8.2f MB/s\n" current_size (fromRat lat :: Double) (fromRat bw :: Double)

    testRoundTrip conn sbuff msg_size = do
        send conn (B.take (fromIntegral msg_size) sbuff) 0 []
        void$ loopWhileM id$ pollWithEventData ep$ \evd ->
          case evd of
            EvRecv bs _conn -> let _ = bs :: ByteString in return False
            EvSend _ _ _    -> return True
            _               -> print evd >> return True


-- | @loopWhileM p io@ performs @io@ repeteadly while its result satisfies @p@.
-- Yields the first offending result.
loopWhileM :: Monad m => (a -> Bool) -> m a -> m a
loopWhileM p m = go
  where go = m >>= \a -> if p a then go else return a

-- | @foreverSt s f@ performs @f@ repeteadly which modifies a state initialized with @s@.
-- Never finishes unless there is an exception.
-- foreverSt :: Monad m => a -> (a -> m a) -> m b 
-- foreverSt a fm = fm a >>= \a' -> foreverSt a' fm

