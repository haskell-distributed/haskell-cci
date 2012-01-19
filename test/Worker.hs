--
-- Copyright (C) 2012 Parallel Scientific. All rights reserved.
--
-- See the accompanying COPYING file for license information.
--

import Prelude hiding          ( catch )
import Control.Exception       ( catch, finally, SomeException )
import Control.Monad           ( void )
import Data.ByteString as B    ( ByteString, concat )
import Data.ByteString.Lazy    ( toChunks, fromChunks )
import Data.Binary             ( decode, encode )
import Data.IORef              ( newIORef, IORef, atomicModifyIORef, readIORef )
import qualified Data.Map as M ( empty, Map, lookup, insert )
import qualified Data.Set as S ( empty, Set, insert, member, delete )
import Data.Word               ( Word64 )
import Foreign.Ptr             ( WordPtr )

import Network.CCI             ( initCCI, withEndpoint, connect, ConnectionAttributes(..)
                               , withEventData, EventData(..), disconnect, send, Connection
                               , accept, reject, Event
                               )

import Commands                ( initCommands,readCommand
                               , Command(ConnectTo, Send, Accept, Reject, Disconnect, Quit)
                               , Response( Error,Recv,ReqAccepted,ReqRejected,ReqIgnored,ConnectAccepted
                                         , SendCompletion, Rejected, TimedOut, KeepAliveTimedOut
                                         , EndpointDeviceFailed, Bye
                                         )
                               , sendResponse
                               )


main :: IO ()
main = flip finally (sendResponse Bye)$ flip catch (\e -> sendResponse$ Error$ "Exception: "++show (e :: SomeException))$ do
    initCCI
    initCommands
    rcm <- emptyConnMap
    rcrs <- emptyConnReq
    withEndpoint Nothing$ \(ep,_fd) -> do
      void$ loopWhileM id$ readCommand >>= \cm ->
         case cm of

           ConnectTo uri i mt ->
               connect ep uri (B.concat$ toChunks$ encode (fromIntegral i :: Word64)) CONN_ATTR_UU i mt

           Accept i -> markAccept i rcrs

           Reject i -> markReject i rcrs

           Disconnect i -> getConn i rcm >>= disconnect

           Send i ctx bs -> 
               getConn i rcm >>= \c -> send c bs ctx []

           WaitEvent -> events rcm rcrs ep >> return True

           Quit -> return False

  where
    events rcm rcrs ep = do
        void$ loopWhileM id$ withEventData ep$ maybe (return True)$ \ev -> do
            case ev of
              EvConnectAccepted ctx conn -> insertConn ctx conn rcm >> sendResponse (ConnectAccepted ctx)
               
              EvSend ctx st conn -> getConnId conn rcm >>= \c -> sendResponse (SendCompletion c ctx st) 

              EvRecv bs conn -> getConnId conn rcm >>= \c -> sendResponse (Recv c bs)

              EvConnectRequest e bs cattrs -> handleConnectionRequest rcm rcrs e bs cattrs

              EvConnectTimedOut ctx -> sendResponse (TimedOut ctx)

              EvConnectRejected ctx -> sendResponse (Rejected ctx)

              EvKeepAliveTimedOut conn -> getConnId conn rcm >>= \c -> sendResponse (KeepAliveTimedOut c)

              EvEndpointDeviceFailed _ -> sendResponse EndpointDeviceFailed

            return False


-- | @loopWhileM p io@ performs @io@ repeteadly while its result satisfies @p@.
-- Yields the first offending result.
loopWhileM :: (a -> Bool) -> IO a -> IO a
loopWhileM p io = io >>= \a -> if p a then loopWhileM p io else return a


-- Map of connection requests

type ConnReqs = IORef ConnReqsD

data ConnReqsD = ConnReqsD
    { connAccept :: S.Set WordPtr
    , connReject :: S.Set WordPtr
    }

emptyConnReq :: IO ConnReqs
emptyConnReq = newIORef$ ConnReqsD S.empty S.empty

markAccept :: WordPtr -> ConnReqs -> IO ()
markAccept i rcrs = atomicModifyIORef rcrs$ 
     \cr -> (cr { connAccept = S.insert i (connAccept cr) } , ())

markReject :: WordPtr -> ConnReqs -> IO ()
markReject i rcrs = atomicModifyIORef rcrs$ 
     \cr -> (cr { connReject = S.insert i (connReject cr) } , ())

handleConnectionRequest :: ConnMap -> ConnReqs -> Event -> ByteString -> ConnectionAttributes -> IO ()
handleConnectionRequest rcrm rcrs ev bs _cattrs = do
    r <- atomicModifyIORef rcrs$ \cr ->
        if S.member w (connAccept cr) then (cr { connAccept = S.delete w (connAccept cr) } , ReqAccepted w)
          else if S.member w (connReject cr) then (cr { connReject = S.delete w (connReject cr) } , ReqRejected w)
            else (cr,ReqIgnored w)
    case r of
      ReqAccepted _ -> accept ev >>= \c -> insertConn w c rcrm >> sendResponse r
      ReqRejected _ -> reject ev >> sendResponse r
      _ -> sendResponse r
  where
    w = (fromIntegral :: Word64 -> WordPtr)$ decode$ fromChunks [bs]


-- Map of connections

type ConnMap = IORef (M.Map WordPtr Connection,M.Map Connection WordPtr)

emptyConnMap :: IO ConnMap
emptyConnMap = newIORef (M.empty,M.empty)

getConn :: WordPtr -> ConnMap -> IO Connection
getConn w rcm = 
    readIORef rcm >>=
        maybe (sendResponse (Error$ "connection "++show w++" was not found in the connection map.") 
                >> ioError (userError "Cannot find word.")
              )
              return
        . M.lookup w . fst


getConnId :: Connection -> ConnMap -> IO WordPtr
getConnId c rcm = 
    readIORef rcm >>=
        maybe (sendResponse (Error$ "The given connection was not found in the connection map.")
                  >> ioError (userError "Cannot find word.")
                )
                return
        . M.lookup c . snd

insertConn :: WordPtr -> Connection -> ConnMap -> IO ()
insertConn w c rcm = atomicModifyIORef rcm $ \(wc,cw) -> ((M.insert w c wc, M.insert c w cw),())


