--
-- Copyright (C) 2012 Parallel Scientific. All rights reserved.
--
-- See the accompanying COPYING file for license information.
--

-- This file implements worker processes.
--
-- A worker process runs a loop which takes commands from a driver process and executes them.
-- The commands are CCI operations that the worker executes upon command reception. The
-- driver communicates with the workers through the standard input and output. The workers
-- communicate among themselves through CCI. 
--
-- The driver process must issue commands to indicate to the workers which connection requests 
-- to accept and reject before the connection requests events arrive. For every command, the
-- worker process sends and Idle response to the driver, so the driver can synchronize all the
-- workers and make bugs reproducible.
-- 
-- Worker processes are spawned by the driver process.
--
-- See test/Props.hs for an example of driver process.

import Prelude hiding          ( catch )
import Control.Exception       ( catch, SomeException )
import Data.ByteString as B    ( ByteString, concat )
import Data.ByteString.Lazy    ( toChunks, fromChunks )
import Data.Binary             ( decode, encode )
import Data.IORef              ( newIORef, IORef, atomicModifyIORef, readIORef )
import qualified Data.Map as M ( empty, Map, lookup, insert )
import qualified Data.Set as S ( empty, Set, insert, member, delete )
import Data.Word               ( Word64 )
import Foreign.Ptr             ( WordPtr )
import System.IO               ( hPutStrLn, stderr )

import Network.CCI             ( initCCI, withEndpoint, connect, ConnectionAttributes(..)
                               , EventData(..), disconnect, send, Connection
                               , accept, reject, Event, Status(..), unsafePackEventBytes
                               , endpointURI, pollWithEventData, tryWithEventData
                               )

import Commands                ( initCommands,readCommand
                               , Command(..)
                               , Response( Error,Recv,ReqAccepted,ReqRejected,ReqIgnored,ConnectAccepted
                                         , SendCompletion, Rejected, TimedOut, KeepAliveTimedOut
                                         , EndpointDeviceFailed, Idle
                                         )
                               )
import qualified Commands as C ( sendResponse )

sendResponse :: Response -> IO ()
sendResponse r = hPutStrLn stderr ("response: "++show r) >> C.sendResponse r

main :: IO ()
main = flip catch (\e -> sendResponse$ Error$ "Exception: "++show (e :: SomeException))$ do
    initCommands
    initCCI
    rcm <- emptyConnMap
    rcrs <- emptyConnReq
    withEndpoint Nothing$ \(ep,_fd) -> do
      endpointURI ep >>= putStrLn
      endpointURI ep >>= hPutStrLn stderr
      processCommands rcm rcrs ep (0::Int)

  where

    processCommands rcm rcrs ep ecount = 
       readCommand >>= \cm -> do

         hPutStrLn stderr$ " command: "++show cm
         case seq ecount cm of

           ConnectTo uri _ i mt -> do
               connect ep uri (B.concat$ toChunks$ encode (fromIntegral i :: Word64)) CONN_ATTR_UU i mt
               sendResponse Idle >> processCommands rcm rcrs ep ecount

           Accept i -> markAccept i rcrs >> sendResponse Idle >> processCommands rcm rcrs ep ecount

           Reject i -> markReject i rcrs >> sendResponse Idle >> processCommands rcm rcrs ep ecount

           Disconnect i -> do
               c <- getConn' i rcm
               disconnect c >> sendResponse Idle >> processCommands rcm rcrs ep ecount

           Send i ctx bs -> do
               c <- getConn' i rcm
               send c bs ctx []
               sendResponse Idle >> processCommands rcm rcrs ep ecount

           ProcessEvent -> sendResponse Idle >> processEvent rcm rcrs ep ecount >>= processCommands rcm rcrs ep

           WaitEvents i -> do
                        sendResponse Idle
                        if i<=ecount then processCommands rcm rcrs ep (ecount-i)
                          else do 
                            sequence_ (replicate (i-ecount) (waitEvent rcm rcrs ep))
                            processCommands rcm rcrs ep 0

           WaitConnection cid -> do
                        mc <- getConn cid rcm
                        sendResponse Idle
                        case mc of
                          Nothing -> waitConnection rcm rcrs ep cid ecount >>= processCommands rcm rcrs ep
                          _       -> processCommands rcm rcrs ep ecount

           Quit -> sendResponse Idle

    processEvent rcm rcrs ep ecount = do
        if ecount>0 then return (ecount-1)
          else pollWithEventData ep (handleEvent rcm rcrs) >> return ecount

    waitEvent rcm rcrs ep = pollWithEventData ep$ handleEvent rcm rcrs

    waitConnection rcm rcrs ep cid ecount = do
            mc <- getConn cid rcm
            case mc of
              Nothing -> do
                    pollWithEventData ep$ handleEvent rcm rcrs
                    waitConnection rcm rcrs ep cid (ecount+1)

              _ -> return ecount

    handleEvent rcm rcrs ev = do
            hPutStrLn stderr$ "   event: "++show ev
            case ev of
              EvAccept ctx (Right conn) ->  insertConn ctx conn rcm >> sendResponse (ConnectAccepted ctx)

              EvConnect ctx (Right conn) -> insertConn ctx conn rcm >> sendResponse (ConnectAccepted ctx)
               
              EvConnect ctx (Left ETIMEDOUT) -> sendResponse (TimedOut ctx)

              EvConnect ctx (Left ECONNREFUSED) -> sendResponse (Rejected ctx)

              EvSend ctx st conn -> getConnId conn rcm >>= \c -> sendResponse (SendCompletion c ctx st) 

              EvRecv bs conn -> do 
                      c   <- getConnId conn rcm
                      bs' <- unsafePackEventBytes bs
                      sendResponse (Recv c bs')

              EvConnectRequest e bs cattrs -> do
                      bs' <- unsafePackEventBytes bs
                      handleConnectionRequest rcrs e bs' cattrs

              EvKeepAliveTimedOut conn -> getConnId conn rcm >>= \c -> sendResponse (KeepAliveTimedOut c)

              EvEndpointDeviceFailed _ -> sendResponse EndpointDeviceFailed

              _ -> sendResponse$ Error$ "unhandled event: " ++ show ev

-- | @loopWhileM p io@ performs @io@ repeteadly while its result satisfies @p@.
-- Yields the first offending result.
loopWhileM :: (a -> Bool) -> IO a -> IO a
loopWhileM p io = io >>= \a -> if p a then loopWhileM p io else return a


-- Map of connection requests

type ConnReqs = IORef ConnReqsD

-- | This is a map that specifies whether a connection request with a specific identifier
-- should be accepted or rejected upon reception.
data ConnReqsD = ConnReqsD
    { connAccept :: S.Set WordPtr -- ^ Requests with these identifiers should be accepted.
    , connReject :: S.Set WordPtr -- ^ Requests with these identifiers should be rejected.
    }

emptyConnReq :: IO ConnReqs
emptyConnReq = newIORef$ ConnReqsD S.empty S.empty

markAccept :: WordPtr -> ConnReqs -> IO ()
markAccept i rcrs = atomicModifyIORef rcrs$ 
     \cr -> (cr { connAccept = S.insert i (connAccept cr) } , ())

markReject :: WordPtr -> ConnReqs -> IO ()
markReject i rcrs = atomicModifyIORef rcrs$ 
     \cr -> (cr { connReject = S.insert i (connReject cr) } , ())

handleConnectionRequest :: ConnReqs -> Event s -> ByteString -> ConnectionAttributes -> IO ()
handleConnectionRequest rcrs ev bs _cattrs = do
    r <- atomicModifyIORef rcrs$ \cr ->
        if S.member w (connAccept cr) then (cr { connAccept = S.delete w (connAccept cr) } , ReqAccepted w)
          else if S.member w (connReject cr) then (cr { connReject = S.delete w (connReject cr) } , ReqRejected w)
            else (cr,ReqIgnored w)
    case r of
      ReqAccepted _ -> accept ev w >> sendResponse r
      ReqRejected _ -> reject ev >> sendResponse r
      _ ->  sendResponse r
  where
    w = (fromIntegral :: Word64 -> WordPtr)$ decode$ fromChunks [bs]


-- Map of connections

type ConnMap = IORef (M.Map WordPtr Connection,M.Map Connection WordPtr)

emptyConnMap :: IO ConnMap
emptyConnMap = newIORef (M.empty,M.empty)

getConn :: WordPtr -> ConnMap -> IO (Maybe Connection)
getConn w rcm = readIORef rcm >>= return . M.lookup w . fst

getConn' :: WordPtr -> ConnMap -> IO Connection
getConn' w rcm = getConn w rcm >>= maybe (do
                                     sendResponse (Error$ "unknown connection: "++show w)
                                     ioError$ userError$ "unknown connection: "++show w
                                   ) return

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


