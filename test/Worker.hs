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
import Control.Monad           ( when )
import Data.ByteString         ( ByteString )
import qualified Data.ByteString as B    ( concat )
import Data.ByteString.Lazy    ( toChunks, fromChunks )
import qualified Data.ByteString.Char8 as B8 ( unpack, pack )
import Data.Binary             ( decode, encode )
import Data.Char               ( isDigit )
import Data.IORef              ( newIORef, IORef, atomicModifyIORef, readIORef )
import qualified Data.Map as M ( empty, Map, lookup, insert )
import qualified Data.Set as S ( empty, Set, insert, member, delete )
import qualified Data.IntSet as IS ( empty, IntSet, insert, member )
import Data.Word               ( Word64 )
import Foreign.Ptr             ( WordPtr )
import System.IO               ( hPutStrLn, stderr )

import Network.CCI             ( withCCI, withEndpoint, connect, ConnectionAttributes(..)
                               , EventData(..), disconnect, send, Connection
                               , accept, reject, Event, Status(..), unsafePackEventBytes
                               , endpointURI, pollWithEventData
                               )

import Commands                ( initCommands,readCommand
                               , Command(..), Msg(..)
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
   withCCI$ do
    rcm <- emptyConnMap
    rcrs <- emptyConnReq
    withEndpoint Nothing$ \(ep,_fd) -> do
      endpointURI ep >>= putStrLn
      endpointURI ep >>= hPutStrLn stderr
      processCommands rcm rcrs ep

  where

    processCommands rcm rcrs ep = 
       readCommand >>= \cm -> do

         hPutStrLn stderr$ " command: "++show cm
         case cm of

           ConnectTo uri _ i mt -> do
               connect ep uri (B.concat$ toChunks$ encode (fromIntegral i :: Word64)) CONN_ATTR_UU i mt
               sendResponse Idle >> processCommands rcm rcrs ep

           Accept i -> markAccept i rcrs >> sendResponse Idle >> processCommands rcm rcrs ep

           Reject i -> markReject i rcrs >> sendResponse Idle >> processCommands rcm rcrs ep

           Disconnect i -> do
               c <- getConn' i rcm
               disconnect c >> sendResponse Idle >> processCommands rcm rcrs ep

           Send i ctx bs -> do
               c <- getConn' i rcm
               send c (msgToByteString bs) ctx []
               sendResponse Idle >> processCommands rcm rcrs ep

           WaitConnection cid -> do
                        waitConnection rcm rcrs ep cid
                        sendResponse Idle
                        processCommands rcm rcrs ep

           WaitSendCompletion cid sid -> do
                        waitSendCompletion rcm rcrs ep cid sid
                        sendResponse Idle
                        processCommands rcm rcrs ep

           WaitRecv cid rid -> do
                        waitRecv rcm rcrs ep cid rid
                        sendResponse Idle
                        processCommands rcm rcrs ep

           Quit -> sendResponse Idle


    waitConnection rcm rcrs ep cid = do
            mc <- getConn cid rcm
            case mc of
              Nothing -> do
                    pollWithEventData ep$ handleEvent rcm rcrs
                    waitConnection rcm rcrs ep cid

              _ -> return ()

    waitRecv rcm rcrs ep cid ri = do
            ci <- getConnInfo' cid rcm
            when (not$ IS.member (fromIntegral ri)$ recvs ci)$ do
                    pollWithEventData ep$ handleEvent rcm rcrs
                    waitRecv rcm rcrs ep cid ri

    waitSendCompletion rcm rcrs ep cid si = do
            ci <- getConnInfo' cid rcm
            when (not$ IS.member (fromIntegral si)$ sendCompletions ci)$ do
                    pollWithEventData ep$ handleEvent rcm rcrs
                    waitSendCompletion rcm rcrs ep cid si

    handleEvent rcm rcrs ev = do
            hPutStrLn stderr$ "   event: "++show ev
            case ev of
              EvAccept ctx (Right conn) ->  insertConn ctx conn rcm >> sendResponse (ConnectAccepted ctx)

              EvConnect ctx (Right conn) -> insertConn ctx conn rcm >> sendResponse (ConnectAccepted ctx)
               
              EvConnect ctx (Left ETIMEDOUT) -> sendResponse (TimedOut ctx)

              EvConnect ctx (Left ECONNREFUSED) -> sendResponse (Rejected ctx)

              EvSend ctx st conn -> do
                      cid <- getConnId conn rcm
                      ci <- getConnInfo' cid rcm
                      insertConnInfo cid (ci { sendCompletions = IS.insert (fromIntegral ctx) (sendCompletions ci) }) rcm
                      sendResponse (SendCompletion cid ctx st) 

              EvRecv bs conn -> do 
                      cid   <- getConnId conn rcm
                      ci <- getConnInfo' cid rcm
                      bs' <- unsafePackEventBytes bs
                      let m@(Msg ctx _) = byteStringToMsg bs'
                      seq ctx$ insertConnInfo cid (ci { recvs = IS.insert (fromIntegral ctx) (recvs ci) }) rcm
                      sendResponse (Recv cid m)

              EvConnectRequest e bs cattrs -> do
                      bs' <- unsafePackEventBytes bs
                      handleConnectionRequest rcrs e bs' cattrs

              EvKeepAliveTimedOut conn -> getConnId conn rcm >>= \c -> sendResponse (KeepAliveTimedOut c)

              EvEndpointDeviceFailed _ -> sendResponse EndpointDeviceFailed

              _ -> sendResponse$ Error$ "unhandled event: " ++ show ev

-- | @loopWhileM p io@ performs @io@ repeteadly while its result satisfies @p@.
-- Yields the first offending result.
-- loopWhileM :: (a -> Bool) -> IO a -> IO a
-- loopWhileM p io = io >>= \a -> if p a then loopWhileM p io else return a

byteStringToMsg :: ByteString -> Msg
byteStringToMsg bs = let (ctxs,rest) = break (not . isDigit)$ B8.unpack bs
                      in if not (null ctxs) && wellFormed ctxs rest
                           then Msg (read ctxs) (length rest)
                           else error$ "error parsing message "++ctxs++": "++rest
  where
    wellFormed ctx (' ':rs) = and$ zipWith (==) (cycle ctx) rs
    wellFormed _ _ = False

msgToByteString :: Msg -> ByteString
msgToByteString (Msg ctx l) = let n = show ctx in B8.pack$ n ++ take l (' ' : cycle n)


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

type ConnMap = IORef (M.Map WordPtr ConnectionInfo,M.Map Connection WordPtr)
data ConnectionInfo = ConnInfo
    { connection :: Connection
    , sendCompletions :: IS.IntSet
    , recvs :: IS.IntSet
    }


emptyConnMap :: IO ConnMap
emptyConnMap = newIORef (M.empty,M.empty)

getConnInfo :: WordPtr -> ConnMap -> IO (Maybe ConnectionInfo)
getConnInfo w rcm = readIORef rcm >>= return . M.lookup w . fst

getConnInfo' :: WordPtr -> ConnMap -> IO ConnectionInfo
getConnInfo' w rcm = getConnInfo w rcm >>= maybe (do
                                     sendResponse (Error$ "unknown connection: "++show w)
                                     ioError$ userError$ "unknown connection: "++show w
                                   ) return


getConn :: WordPtr -> ConnMap -> IO (Maybe Connection)
getConn w rcm = getConnInfo w rcm >>= return . fmap connection

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
insertConn w c rcm = insertConnInfo w (ConnInfo c IS.empty IS.empty) rcm

insertConnInfo :: WordPtr -> ConnectionInfo -> ConnMap -> IO ()
insertConnInfo w ci rcm = atomicModifyIORef rcm $ \(wc,cw) -> ((M.insert w ci wc, M.insert (connection ci) w cw),())

