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
-- See test/test_cci.hs for an example of driver process.
{-# LANGUAGE ForeignFunctionInterface   #-}

import Prelude hiding          ( catch )
import Control.Exception       ( catch, SomeException )
import Control.Monad           ( when )
import Data.Binary             ( decode, encode )
import Data.ByteString         ( ByteString )
import qualified Data.ByteString as B    ( concat, length, null, drop )
import Data.ByteString.Lazy    ( toChunks, fromChunks )
import qualified Data.ByteString.Char8 as B8 ( unpack, pack, break )
import Data.Char               ( isDigit, isSpace )
import Data.IORef              ( newIORef, IORef, atomicModifyIORef, readIORef, writeIORef )
import qualified Data.Map as M ( empty, lookup, insert, delete )
import Data.Map                ( Map )
import Data.Maybe              ( isNothing )
import qualified Data.Set as S ( empty, insert, member, delete )
import Data.Set      ( Set )
import qualified Data.IntSet as IS ( empty, IntSet, insert, member, delete )
import Data.Word               ( Word64 )
import Foreign.Ptr             ( WordPtr, Ptr )
import Foreign.C.String        ( castCharToCChar, castCCharToChar )
import Foreign.C.Types         ( CInt(..), CChar )
import Foreign.Storable        ( peek )
import Foreign.Marshal.Alloc   ( alloca )
import Foreign.Marshal.Array   ( pokeArray, peekArray )
import System.IO               ( hPutStrLn, stderr )

import Network.CCI             ( withCCI, withPollingEndpoint, connect, ConnectionAttributes(..)
                               , EventData(..), disconnect, send, Connection
                               , accept, reject, Event, Status(..), unsafePackEventBytes
                               , getEndpt_URI, pollWithEventData, RMALocalHandle, RMARemoteHandle
                               , RMA_MODE(..), RMALocalHandle, RMARemoteHandle, rmaRegister
                               , rmaHandle2ByteString, createRMARemoteHandle, Endpoint, rmaWrite
                               , rmaRead, packEventBytes
                               )

import Commands                ( initCommands,readCommand, msgToString
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
    rmar <- emptyRMAState
    withPollingEndpoint Nothing$ \ep -> do
      getEndpt_URI ep >>= putStrLn
      getEndpt_URI ep >>= hPutStrLn stderr
      processCommands rcm rcrs rmar ep

  where

    processCommands rcm rcrs rmar ep = 
       readCommand >>= \cm -> do
         let go = processCommands rcm rcrs rmar ep
         hPutStrLn stderr$ " command: "++show cm
         case cm of

           ConnectTo uri _ i mt -> do
               connect ep uri (B.concat$ toChunks$ encode (fromIntegral i :: Word64)) CONN_ATTR_RO i mt
               sendResponse Idle >> go

           Accept i -> markAccept i rcrs >> sendResponse Idle >> go

           Reject i -> markReject i rcrs >> sendResponse Idle >> go

           Disconnect i -> do
               c <- getConn' i rcm
               disconnect c >> sendResponse Idle >> go

           Send i ctx bs -> do
               c <- getConn' i rcm
               send c (B8.pack$ msgToString bs) ctx
               sendResponse Idle >> go

           WaitConnection cid -> do
                        waitConnection rmar rcm rcrs ep cid
                        sendResponse Idle
                        go

           WaitSendCompletion cid sid -> do
                        waitSendCompletion rmar rcm rcrs ep cid sid
                        sendResponse Idle
                        go

           WaitRecv cid rid -> do
                        waitRecv rmar rcm rcrs ep cid rid
                        sendResponse Idle
                        go

           RMAReuseRMAHandle cid -> do
                        markReuseRMAH cid rmar
                        sendResponse Idle
                        go

           RMAHandleExchange cid sid -> do
                        h <- createRMALocalHandle ep cid rmar
                        c <- getConn' cid rcm
                        bh <- rmaHandle2ByteString h
                        send c (B8.pack$ msgToString$ MsgRMAH$ bh) sid
                        insertRMAHandleSendId cid sid rmar
                        sendResponse Idle
                        go

           RMAWaitExchange cid -> do
                        waitRMAExchange rmar rcm rcrs ep cid
                        sendResponse Idle
                        go

           RMAPrepareRead cid ctx -> do
               (_,ptr,n) <- getRMALocalHandle cid rmar
               pokeArray ptr (map castCharToCChar$ take n$ cycle$ show ctx)
               sendResponse Idle >> go

           RMAWrite cid ctx -> do
               c <- getConn' cid rcm
               (lh,ptr,n) <- getRMALocalHandle cid rmar
               Just rh <- getRMARemoteHandle cid rmar
               pokeArray ptr (map castCharToCChar$ take n$ cycle$ show ctx)
               rmaWrite c (Just (B8.pack$ "rmaWrite "++show ctx)) rh 0 lh 0 (fromIntegral n) ctx []
               sendResponse Idle >> go

           RMARead cid ctx -> do
               ci <- getConnInfo' cid rcm
               (lh,_,n) <- getRMALocalHandle cid rmar
               Just rh <- getRMARemoteHandle cid rmar
               insertConnInfo cid ci { rmaReadIds = IS.insert (fromIntegral ctx) (rmaReadIds ci) } rcm
               rmaRead (connection ci) (Just (B8.pack$ "rmaRead "++show ctx)) lh 0 rh 0 (fromIntegral n) ctx []
               sendResponse Idle >> go

           RMAWaitWrite cid ctx -> do
               waitRMAWrite rmar rcm rcrs ep cid ctx
               sendResponse Idle
               go

           RMAWaitRead cid ctx -> do
               waitRMARead rmar rcm rcrs ep cid ctx
               sendResponse Idle
               go

           RMAFreeHandles cid -> do
               freeRMALocalHandle cid rmar
               sendResponse Idle >> go

           Quit -> sendResponse Idle


    waitConnection rmar rcm rcrs ep cid = do
            mc <- getConn cid rcm
            case mc of
              Nothing -> do
                    pollWithEventData ep$ handleEvent rmar rcm rcrs
                    waitConnection rmar rcm rcrs ep cid

              _ -> return ()

    waitRecv rmar rcm rcrs ep cid ri = do
            ci <- getConnInfo' cid rcm
            when (not$ IS.member (fromIntegral ri)$ recvs ci)$ do
                    pollWithEventData ep$ handleEvent rmar rcm rcrs
                    waitRecv rmar rcm rcrs ep cid ri

    waitSendCompletion rmar rcm rcrs ep cid si = do
            ci <- getConnInfo' cid rcm
            when (not$ IS.member (fromIntegral si)$ sendCompletions ci)$ do
                    pollWithEventData ep$ handleEvent rmar rcm rcrs
                    waitSendCompletion rmar rcm rcrs ep cid si

    waitRMAWrite rmar rcm rcrs ep cid si = do
            ci <- getConnInfo' cid rcm
            written <- testRMAWriteId cid rmar
            when (not written && not (IS.member (fromIntegral si)$ sendCompletions ci))$ do
                    pollWithEventData ep$ handleEvent rmar rcm rcrs
                    waitRMAWrite rmar rcm rcrs ep cid si

    waitRMARead rmar rcm rcrs ep cid si = do
            ci <- getConnInfo' cid rcm
            when (not (IS.member (fromIntegral si)$ recvs ci)
                  && not (IS.member (fromIntegral si)$ sendCompletions ci))$ do
                    pollWithEventData ep$ handleEvent rmar rcm rcrs
                    waitRMARead rmar rcm rcrs ep cid si

    waitRMAExchange rmar rcm rcrs ep cid = do
            mr <- getRMARemoteHandle cid rmar
            Just si <- getRMAHandleSendId cid rmar 
            ci <- getConnInfo' cid rcm
            when (isNothing mr || (not$ IS.member (fromIntegral si)$ sendCompletions ci))$ do
                    pollWithEventData ep$ handleEvent rmar rcm rcrs
                    waitRMAExchange rmar rcm rcrs ep cid

    handleEvent rmar rcm rcrs ev = do
            hPutStrLn stderr$ "   event: "++show ev
            case ev of
              EvAccept ctx (Right conn) ->  insertConn ctx conn rcm >> sendResponse (ConnectAccepted ctx)

              EvConnect ctx (Right conn) -> insertConn ctx conn rcm >> sendResponse (ConnectAccepted ctx)
               
              EvConnect ctx (Left ETIMEDOUT) -> sendResponse (TimedOut ctx)

              EvConnect ctx (Left ECONNREFUSED) -> sendResponse (Rejected ctx)

              EvSend ctx st conn -> do
                      cid <- getConnId conn rcm
                      ci <- getConnInfo' cid rcm
                      when (ctx/=0)$ insertConnInfo cid ci { sendCompletions = IS.insert (fromIntegral ctx) (sendCompletions ci) } rcm
                      when (IS.member (fromIntegral ctx) (rmaReadIds ci))$ do
                          (_,ptr,n) <- getRMALocalHandle cid rmar
                          checkRMABuffer ptr n ctx
                      sendResponse (SendCompletion cid ctx st) 

              EvRecv bs conn -> do 
                      cid   <- getConnId conn rcm
                      ci <- getConnInfo' cid rcm
                      bs' <- packEventBytes bs
                      let m = byteStringToMsg bs' 
                      case m of 
                        Msg ctx _ -> seq ctx$ insertConnInfo cid (ci { recvs = IS.insert (fromIntegral ctx) (recvs ci) }) rcm
                        MsgRMARead ctx -> seq ctx$ insertConnInfo cid (ci { recvs = IS.insert (fromIntegral ctx) (recvs ci) }) rcm
                        MsgRMAH rh -> insertRMARemoteHandle cid (maybe (error "handleEvent: MsgRMAH") id$ createRMARemoteHandle rh) rmar
                        MsgRMAWrite ctx -> do 
                            (_,ptr,n) <- getRMALocalHandle cid rmar
                            checkRMABuffer ptr n ctx
                            insertRMAWriteId cid rmar
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
byteStringToMsg bs = let (ctxs,rest) = B8.break isSpace bs
                      in if not (B.null ctxs) && wellFormed (B8.unpack ctxs) (B8.unpack rest)
                           then case B8.unpack ctxs of
                                  "rmaH"     -> MsgRMAH$ B.drop 1 rest
                                  "rmaRead"  -> MsgRMARead$ read$ drop 1$ B8.unpack rest
                                  "rmaWrite" -> MsgRMAWrite$ read$ drop 1$ B8.unpack rest
                                  _          -> Msg (read$ B8.unpack ctxs) (B.length bs)
                           else error$ "error parsing message "++show ctxs++" (length: "++show (B.length bs)++"): "++show rest
  where
    wellFormed "rmaH" (' ':rs) = length rs==32
    wellFormed "rmaRead" (' ':rs) = all isDigit rs
    wellFormed "rmaWrite" (' ':rs) = all isDigit rs
    wellFormed ctx (' ':rs) | all isDigit ctx = and$ zipWith (==) (cycle ctx) rs
    wellFormed _ _ = False

checkRMABuffer :: Ptr CChar -> Int -> WordPtr -> IO ()
checkRMABuffer ptr n ctx = do
    msg <- fmap (map castCCharToChar)$ peekArray n ptr
    when (msg/= (take n$ cycle$ show ctx))$ ioError$ userError$ "checkRMABuffer: "++msg

-- Map of connection requests

type ConnReqs = IORef ConnReqsD

-- | This is a map that specifies whether a connection request with a specific identifier
-- should be accepted or rejected upon reception.
data ConnReqsD = ConnReqsD
    { connAccept :: Set WordPtr -- ^ Requests with these identifiers should be accepted.
    , connReject :: Set WordPtr -- ^ Requests with these identifiers should be rejected.
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

type ConnMap = IORef (Map WordPtr ConnectionInfo,Map Connection WordPtr)
data ConnectionInfo = ConnInfo
    { connection :: Connection
    , sendCompletions :: IS.IntSet
    , rmaReadIds :: IS.IntSet
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

-- | Fails with an error if the connection is not in the map.
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
insertConn w c rcm = insertConnInfo w (ConnInfo c IS.empty IS.empty IS.empty) rcm

insertConnInfo :: WordPtr -> ConnectionInfo -> ConnMap -> IO ()
insertConnInfo w ci rcm = atomicModifyIORef rcm $ \(wc,cw) -> ((M.insert w ci wc, M.insert (connection ci) w cw),())

----------------------
-- RMA state
----------------------

data RMAState = RMAState 
    { reused :: Set WordPtr
    , reservedLocalHandles :: Map WordPtr (RMALocalHandle,Ptr CChar,Int)
    , availableHandles :: [(RMALocalHandle,Ptr CChar,Int)]
    , remoteHandles :: Map WordPtr RMARemoteHandle
    , localHandlesSendIds :: Map WordPtr WordPtr
    , rmaWritesIds :: IS.IntSet
    }

emptyRMAState :: IO (IORef RMAState)
emptyRMAState = newIORef RMAState 
    { reused        = S.empty
    , availableHandles  = []
    , remoteHandles = M.empty
    , localHandlesSendIds = M.empty
    , reservedLocalHandles = M.empty
    , rmaWritesIds = IS.empty 
    }

markReuseRMAH :: WordPtr -> IORef RMAState -> IO ()
markReuseRMAH w r = atomicModifyIORef r (\rmas -> (rmas { reused = S.insert w (reused rmas) },()))


createRMALocalHandle :: Endpoint -> WordPtr -> IORef RMAState -> IO RMALocalHandle
createRMALocalHandle ep cid rmar = do
    rmas <- readIORef rmar
    let h@(lh',_,_):hss = availableHandles rmas
    lh <- if S.member cid (reused rmas) && not (null$ availableHandles rmas)
      then do
        writeIORef rmar 
            rmas { reused = S.delete cid (reused rmas)
                 , availableHandles = hss
                 , reservedLocalHandles = M.insert cid h (reservedLocalHandles rmas)
                 }
        return lh'

      else do
        ptr <- alloca$ \pptr -> posix_memalign pptr 4096 4096 >> peek pptr
        lh <- rmaRegister ep (ptr,4096) RMA_READ_WRITE
        writeIORef rmar 
            rmas { reservedLocalHandles = M.insert cid (lh,ptr,4096) (reservedLocalHandles rmas)
                 }
        return lh

    return lh
 
insertRMARemoteHandle :: WordPtr -> RMARemoteHandle -> IORef RMAState -> IO ()
insertRMARemoteHandle cid rh rmar = do
    atomicModifyIORef rmar$ \rmas -> ( rmas { remoteHandles = M.insert cid rh (remoteHandles rmas) } , ())

getRMARemoteHandle :: WordPtr -> IORef RMAState -> IO (Maybe RMARemoteHandle)
getRMARemoteHandle cid rmar = readIORef rmar >>= return . M.lookup cid . remoteHandles
  
getRMAHandleSendId :: WordPtr -> IORef RMAState -> IO (Maybe WordPtr)
getRMAHandleSendId cid rmar = readIORef rmar >>= return . M.lookup cid . localHandlesSendIds
  
getRMALocalHandle :: WordPtr -> IORef RMAState -> IO (RMALocalHandle,Ptr CChar,Int)
getRMALocalHandle cid rmar = readIORef rmar >>= return . maybe (error "getRMALocalHandle") id . M.lookup cid . reservedLocalHandles
  
freeRMALocalHandle :: WordPtr -> IORef RMAState -> IO ()
freeRMALocalHandle cid rmar = atomicModifyIORef rmar$ \rmas ->
    let h = maybe (error "freeRMALocalHandle") id$ M.lookup cid (reservedLocalHandles rmas)
     in ( rmas { availableHandles = availableHandles rmas ++ [ h ] 
               , reservedLocalHandles = M.delete cid (reservedLocalHandles rmas)
               , remoteHandles = M.delete cid (remoteHandles rmas)
               } 
        , ()
        )

insertRMAHandleSendId :: WordPtr -> WordPtr -> IORef RMAState -> IO ()
insertRMAHandleSendId cid sid rmar = atomicModifyIORef rmar (\rmas -> (rmas { localHandlesSendIds = M.insert cid sid (localHandlesSendIds rmas) }, ()))

insertRMAWriteId :: WordPtr -> IORef RMAState -> IO ()
insertRMAWriteId cid rmar = atomicModifyIORef rmar (\rmas -> (rmas { rmaWritesIds = IS.insert (fromIntegral cid) (rmaWritesIds rmas) }, ()))

testRMAWriteId :: WordPtr -> IORef RMAState -> IO Bool
testRMAWriteId cid rmar = atomicModifyIORef rmar (\rmas -> (rmas { rmaWritesIds = IS.delete (fromIntegral cid) (rmaWritesIds rmas) }
                                                           , IS.member (fromIntegral cid)$ rmaWritesIds rmas
                                                           )
                                                 )

--  int posix_memalign(void **memptr, size_t alignment, size_t size);
foreign import ccall "static stdlib.h" posix_memalign :: Ptr (Ptr CChar) -> CInt -> CInt -> IO CInt 

