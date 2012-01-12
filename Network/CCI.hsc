{-# LANGUAGE DeriveDataTypeable         #-} 
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ForeignFunctionInterface   #-}

-- | Haskell bindings for CCI. 
--
-- See <http://www.olcf.ornl.gov/center-projects/common-communication-interface> .
-- Most of the comments in this module has been taken and reedited from there.
--
module Network.CCI
  ( initCCI
  -- * Devices
  , Device
  , DevicesHandle
  , getDevices
  , freeDevices
  , withDevices
  -- * Endpoints
  , Endpoint
  , createEndpoint
  , destroyEndpoint
  , withEndpoint
  , endpointURI
  -- * Connections
  , Connection
  , connectionMaxSendSize
  , accept
  , reject
  , connect
  , disconnect
  , ConnectionAttributes(..)
  , WordPtr
  -- * Data transfers
  -- $dt

  -- ** Active Messages
  -- $amsg
  , send
  , sendv
  , SEND_FLAG(..)
  -- ** RMA
  -- $rma
  , rmaEndpointRegister
  , rmaConnectionRegister 
  , rmaDeregister 
  , withEndpointRMALocalHandle 
  , withConnectionRMALocalHandle
  , rmaRead
  , rmaWrite
  , RMA_FLAG(..)
  , RMALocalHandle
  , RMARemoteHandle
  , rmaHandle2ByteString
  , createRMARemoteHandle
  -- * Event handling
  , getEvent
  , returnEvent
  , tryWithEventData
  , pollWithEventData
  , withEventData
  , Event
  , getEventData
  , BufferHandler(..)
  , VolatilByteString(..)
  , EventData(..)
  -- * Setting options
  , setConnectionOpt
  , setEndpointOpt
  , getConnectionOpt
  , getEndpointOpt
  , EndpointOption(..)
  , ConnectionOption(..)
  -- * Error handling
  , strError 
  , CCIException(..)
  , Status(..)
  ) where

import Control.Concurrent     ( threadWaitRead )
import Control.Exception      ( Exception, throwIO, bracket, onException, mask_ )
import Control.Monad          ( liftM2, liftM3, join )
import Data.Bits              ( (.|.), shiftR, shiftL, (.&.) )
import Data.ByteString as B   ( ByteString, packCStringLen, unpack, pack, length )
import Data.ByteString.Unsafe ( unsafePackCStringLen, unsafeUseAsCStringLen )
import Data.Dynamic           ( Typeable )
import Data.Word              ( Word8, Word32, Word64 )
import Foreign.C.Types        ( CInt, CChar )
import Foreign.C.String       ( CString, peekCString, CStringLen, withCString )
import Foreign.Ptr            ( Ptr, nullPtr, WordPtr, wordPtrToPtr, plusPtr, ptrToWordPtr )
import Foreign.Marshal.Alloc  ( alloca, allocaBytes )
import Foreign.Storable       ( Storable(peek, poke, peekElemOff, pokeByteOff, peekByteOff, sizeOf) )

import System.Posix.Types     ( Fd(Fd) )


#include <cci.h>


-- | Initializes CCI. If called more than once, only the
--   first call has any effects.
--
--   This function will attempt to load the configuration file pointed by
--   the CCI_CONFIG environment variable.
--   A summary of the format of this configuration file follows.
--
-- > # Comments are anything after the # symbols.
-- > # Sections in this file are denoted by [section name]. Each section
-- > # denotes a single CCI device.
-- >  
-- > [bob0]
-- > # The only mandated field in each section is "driver". It indicates
-- > # which CCI driver should be applied to this device.
-- > driver = psm
-- >
-- > # The priority field determines the ordering of devices returned by
-- > # getDevices. 100 is the highest priority; 0 is the lowest priority.
-- > # If not specified, the priority value is 50.
-- > priority = 10
-- >
-- > # The last field understood by the CCI core is the "default" field.
-- > # Only one device is allowed to have a "true" value for default. All
-- > # others must be set to 0 (or unset, which is assumed to be 0). If
-- > # one device is marked as the default, then this device will be used
-- > # when NULL is passed as the device when creating an endpoint. If no
-- > # device is marked as the default, it is undefined as to which device
-- > # will be used when NULL is passed as the device when creating an
-- > # endpoint.
-- > default = 1
-- > 
-- > # All other fields are uninterpreted by the CCI core; they're just
-- > # passed to the driver. The driver can do whatever it wants with
-- > # these values (e.g., system admins can set values to configure the
-- > # driver). Driver documentation should specify what parameters are
-- > # available, what each parameter is/does, and what its legal values
-- > # are.
-- > 
-- > # This example shows a bonded PSM device that uses both the ipath0 and
-- > # ipath1 devices. Some other parameters are also passed to the PSM
-- > # driver; it assumedly knows how to handle them.
-- > 
-- > device = ipath0,ipath1
-- > capabilities = bonded,failover,age_of_captain:52
-- > qos_stuff = fast
-- > 
-- > # bob2 is another PSM device, but it only uses the ipath0 device.
-- > [bob2]
-- > driver = psm
-- > device = ipath0
-- > 
-- > # bob3 is another PSM device, but it only uses the ipath1 device.
-- > [bob3]
-- > driver = psm
-- > device = ipath1
-- > sl = 3 # IB service level (if applicable)
-- > 
-- > # storage is a device that uses the sock driver (udp sockets). Note
-- > # that this driver allows specifying which device to use by specifying 
-- > # its IP address and MAC address -- assumedly it's an error if there is
-- > # no single device that matches both the specified IP address and MAC
-- > # (vs. specifying a specific device name).
-- > [storage]
-- > driver = sock
-- > priority = 5
-- > ip = 172.31.194.1
-- > mac = 01:12:23:34:45
--
-- On a debian PC this is the configuration which worked for us:
--
-- > [eth0]
-- > driver = sock
-- > # replace with your device ip address
-- > ip = 192.168.0.1
-- > # replace with your device mac address
-- > mac = 00:e0:7d:ad:95:5e
-- > default = 1
-- 
-- May throw:
--
--  * 'ENOMEM' Not enough memory to complete.
--
--  * 'ERR_NOT_FOUND' No driver or CCI_CONFIG.
--
--  * 'ERROR' Unable to parse CCI_CONFIG.
--
--  * errno errors if fopen() fails.
--
--  * driver-specific errors.
initCCI :: IO ()
initCCI = alloca$ \p -> poke p 0 >> cci_init #{const CCI_ABI_VERSION} 0 p  >>= cci_check_exception

foreign import ccall unsafe cci_init :: Word32 -> Word32 -> Ptr Word32 -> IO CInt



------------------------------------------
-- Devices
------------------------------------------

-- | A device represents a network interface card (NIC) or host channel adapter 
-- (HCA) that connects the host and network. A device may provide multiple 
-- endpoints (typically one per core).
--
newtype Device = Device (Ptr Device)

-- | Handle used to free devices
newtype DevicesHandle = DevicesHandle (Ptr (Ptr Device))


-- | Returns a list of \"up\" devices.
--
-- The order of devices returned corresponds to the priority fields in
-- the devices. If two devices share the same priority, their
-- ordering in the return array is arbitrary.
--
-- Use the returned handle to freed resources associated with
-- the devices using 'freeDevices'.
--
-- Use 'withDevices' instead when it fits your purposes.
getDevices :: IO ([Device],DevicesHandle)
getDevices = do
    pdev <- alloca$ \ppdev -> cci_get_devices ppdev >>= cci_check_exception >> peek ppdev
    devices <- peekNullTerminated pdev 0
    return (map Device devices,DevicesHandle pdev)
  where
    peekNullTerminated :: Ptr (Ptr Device) -> Int -> IO [Ptr Device]
    peekNullTerminated p i = do
      d <- peekElemOff p i
      if d == nullPtr then return []
        else fmap (d:)$ peekNullTerminated p (i+1) 


foreign import ccall unsafe cci_get_devices :: Ptr (Ptr (Ptr Device)) -> IO CInt


-- | Resources used by devices obtained with 'getDevices' are freed 
-- with this call.
--
-- Calling this on a DevicesHandles which has been freed already
-- has no effect.
--
-- Drivers may throw some error when freeing devices.
--
freeDevices :: DevicesHandle -> IO ()
freeDevices (DevicesHandle p) = cci_free_devices p >>= cci_check_exception

foreign import ccall unsafe cci_free_devices :: Ptr (Ptr Device) -> IO CInt


-- | Calls 'getDevices' and 'freeDevices' around a given
-- block of code. in the presence of errors, 'freeDevices'
-- is guaranteed to be called.
withDevices :: ([Device] -> IO a) -> IO a
withDevices f = bracket getDevices (freeDevices . snd) (f . fst)




------------------------------------------
--
-- Endpoints
--
------------------------------------------


-- | In CCI, an endpoint is the process virtual instance of a device. The endpoint 
-- is the container of all the communication resources needed by the process 
-- including queues and buffers including shared send and receive buffers. A 
-- single endpoint may communicate with any number of peers and provides a single 
-- completion queue regardless of the number of peers. CCI achieves better 
-- scalability on very large HPC and data center deployments since the endpoint 
-- manages buffers independent of how many peers it is communicating with.
-- 
-- CCI uses an event model in which an application may either poll or wait for the 
-- next event.  Events include communication (e.g. send completions)  as well as 
-- connection handling (e.g. incoming client connection requests). The application 
-- should return the events to CCI when it no longer needs them. CCI 
-- achieves better scalability in time versus Sockets since all events are managed 
-- by a single completion queue.
--
-- Because an endpoint serves many connections, some events carry a so-called 
-- context value which is user-defined and helps identifying with which connection 
-- and transfer operation the event is related. 
--
-- In this realization of the API, all events related to an endpoint use the same type
-- for context values, which is expressed as the @ctx@ type parameter of 'Endpoint's,
-- 'Connection's and 'Event's.
--
newtype Endpoint = Endpoint (Ptr EndpointV)
  deriving (Storable, Show)

data EndpointV


-- | This function creates a CCI endpoint.
-- An endpoint is associated with a device that performs the
-- actual communication (see 'getDevices').
--
-- If it is desirable to bind the CCI endpoint to a specific set of
-- resources (e.g., a NUMA node), you should bind the calling thread
-- before calling 'createEndpoint'. This is probably done by starting
-- the thread with 'Control.Concurrent.forkOn'.
--
-- The OS handle returned by createEndpoint can be manipulated with
-- 'Control.Concurrent.threadWaitRead' and 'Control.Concurrent.threadWaitWrite'.
--
-- The garbage collector will release resources associated with the handle
-- if all references to the endpoint are lost and the memory it uses is
-- ever claimed. Additionally, you can call 'destroyEndpoint' to avoid 
-- depending on the garbage collector for that sake.
--
-- May throw:
--
--  * 'ENODEV' if the device is not \"up\"
--
--  * 'ENOMEM' if not enough memory is available
--
--  * driver-specific errors
--
createEndpoint :: Maybe Device -- ^ The device to use or Nothing to use the system-default device.
               -> IO (Endpoint,Fd) -- ^ The endpoint and an operating system handle that can be used
                                   -- to block for progress on this endpoint.
createEndpoint mdev = alloca$ \ppend ->
    alloca$ \pfd -> do
      cci_create_endpoint (maybe nullPtr (\(Device pdev) -> pdev) mdev) 0 ppend pfd >>= cci_check_exception
      liftM2 ((,)) (peek ppend) (fmap Fd$ peek pfd)

foreign import ccall unsafe cci_create_endpoint :: Ptr Device -> CInt -> Ptr Endpoint -> Ptr CInt -> IO CInt


-- | Frees resources associated with the endpoint. All open connections 
-- are closed immediately. It is exactly as if 'disconnect' was 
-- invoked on every open connection on this endpoint.
--
-- May throw driver-specific errors.
destroyEndpoint :: Endpoint -> IO ()
destroyEndpoint (Endpoint pend) = cci_destroy_endpoint pend >>= cci_check_exception

foreign import ccall unsafe cci_destroy_endpoint :: Ptr EndpointV -> IO CInt


-- | Wraps an IO action with calls to 'createEndpoint' and 'destroyEndpoint'.
--
-- Makes sure that 'destroyEndpoint' is called in the presence of errors.
--
withEndpoint :: Maybe Device -> ((Endpoint,Fd) -> IO a) -> IO a
withEndpoint mdev = bracket (createEndpoint mdev) (destroyEndpoint . fst)


-- | Driver created URI of the endpoint. May be passed to clients out-of-band
-- to pass to 'connect'. The application should never need to parse this URI.
endpointURI :: Endpoint -> IO String
endpointURI (Endpoint pend) = #{peek cci_endpoint_t, name} pend >>= peekCString


------------------------------------------
--
--  Connections
--
------------------------------------------


-- | CCI uses connections to allow an application to choose the level
-- of service that best fits its needs and to provide fault isolation 
-- should a peer stop responding. CCI does not, however, allocate 
-- buffers per peer which reduces scalability.
--
-- CCI offers choices of reliability and ordering. Some applications 
-- such as distributed filesystems need to know that data has arrived 
-- at the peer. A health monitoring application, on the other hand, 
-- may want to send data that has a very short lifetime and does not 
-- want to expend any efforts on retransmission. CCI can accommodate both.
--
-- CCI provides Unreliable-Unordered (UU), Reliable-Unordered (RU), 
-- and Reliable-Ordered (RO) connections as well as UU with send 
-- multicast and UU with receive multicast. Most RPC style applications 
-- do not require ordering and can take advantage of RU connections.
-- When ordering is not required, CCI can take better advantage of 
-- multiple network paths, etc.
--
-- This datatype represents connections for endpoints with contexts
-- of type @ctx@.
--
newtype Connection = Connection (Ptr ConnectionV)
  deriving (Storable, Show, Ord, Eq)

data ConnectionV


-- | Maximum size of the messages the connection can send.
connectionMaxSendSize :: Connection -> IO Word32
connectionMaxSendSize (Connection pconn) = #{peek cci_connection_t, max_send_size} pconn


-- | Accepts a connection request and establish a connection with a 
-- specific endpoint.
--
-- May throw:
--
--  * 'EINVAL' if the Event was not a EvConnectRequest
--
--  * driver-specific errors
--
accept :: Event  -- ^ A connection request event previously returned by 'getEvent'
       -> IO Connection
accept (Event penv) = alloca$ \ppconn ->
    cci_accept penv ppconn >>= cci_check_exception >> peek ppconn

foreign import ccall unsafe cci_accept :: Ptr EventV -> Ptr Connection -> IO CInt



-- | Rejects a connection request.
--
-- May throw:
--
--  * 'EINVAL' if the Event was not a 'EvConnectRequest'
--
--  * driver-specific errors
--
reject :: Event -> IO ()
reject (Event penv) = cci_reject penv >>= cci_check_exception

foreign import ccall unsafe cci_reject :: Ptr EventV -> IO CInt


-- | Initiate a connection request (client side).
--
-- Request a connection from a specific endpoint. The server endpoint's address
-- is described by a Uniform Resource Identifier. The use of an URI allows for
-- flexible description (IP address, hostname, etc).
--
-- The connection request can carry limited amount of data to be passed to the
-- server for application-specific usage (identification, authentication, etc).
-- Implementations must support lengths <= 1,024 bytes.
--
-- The connect call is always non-blocking, reliable and requires a decision
-- by the server (accept or reject), even for an unreliable connection, except
-- for multicast.
--
-- Multicast connections don't necessarily involve a discrete connection
-- server, they may be handled by IGMP or other distributed framework.
--
-- endpoint Local endpoint to use for requested connection.
--
-- May throw device-specific errors.
--
connect :: Endpoint     -- ^ Local endpoint to use for requested connection.
        -> String       -- ^ Uniform Resource Identifier of the server. It is
                        --   accessible when the server's endpoint is created.
                        --
                        --   Examples:
                        --
                        --     * IP address: \"ip://172.31.194.2\"
                        --
                        --     * Resolvable name: \"ip://foo.bar.com\"
                        --
                        --     * IB LID or GID: \"ib://TBD\"
                        --
                        --     * Blah id: \"blah://crap0123\"
                        --
                        --     * With arguments: \"ip://foo.bar.com:eth1,eth3\"

        -> ByteString   -- ^ Connection data to be send in the connection request
                        --   (for authentication, etc). 
        -> ConnectionAttributes -- ^ Attributes of the requested connection (reliability,
                                --   ordering, multicast, etc).
        -> WordPtr        -- ^ Pointer-sized integer used as context to identify the connection later.
        -> Maybe Word64   -- ^ Timeout to wait for a connection in microseconds. Nothing means \"forever\". 
        -> IO ()
connect (Endpoint pend) uri bdata ca pctx mtimeout = withCString uri$ \curi ->
   unsafeUseAsCStringLen bdata$ \(cdata,clen) -> do
    let fconnect ptv = cci_connect pend curi cdata (fromIntegral clen) 
                                   (fromIntegral$ fromEnum ca) (wordPtrToPtr pctx) 0 ptv 
                         >>= cci_check_exception
    case mtimeout of
      Nothing -> fconnect nullPtr
      Just ts -> 
        let (sec,usec) = divMod ts (10^(6::Int))
         in allocaBytes #{size struct timeval}$ \ptv -> do
              #{poke struct timeval, tv_sec} ptv sec
              #{poke struct timeval, tv_usec} ptv usec
              fconnect ptv

foreign import ccall unsafe cci_connect :: Ptr EndpointV -> CString -> Ptr CChar -> Word32 -> CInt -> Ptr () -> CInt -> Ptr () -> IO CInt


-- | Tears down an existing connection.
--
-- Operation is local, remote side is not notified. From that point,
-- both local and remote side will get a 'ERR_DISCONNECTED' communication error
-- if sends are initiated on this connection.
--
-- May throw driver-specific errors.
disconnect :: Connection -> IO ()
disconnect (Connection pconn) = cci_disconnect pconn >>= cci_check_exception

foreign import ccall unsafe cci_disconnect :: Ptr ConnectionV -> IO CInt


-- | Connection characteristics.
data ConnectionAttributes =
    -- | Reliable ordered. Means that both completions and delivery are
    -- in the same order that they were issued.
    CONN_ATTR_RO
    -- | Reliable unordered. Means that delivery is guaranteed, but both
    -- delivery and completion may be in a different order than they were
    -- issued.
  | CONN_ATTR_RU
    -- | Unreliable unordered (RMA forbidden). Delivery is not guaranteed,
    -- and both delivery and completions may be in a different order 
    -- than they were issued.
  | CONN_ATTR_UU
    -- Multicast send (RMA forbidden).
  | CONN_ATTR_UU_MC_TX
    -- Multicast rcv (RMA forbidden).
  | CONN_ATTR_UU_MC_RX

 deriving Show


instance Enum ConnectionAttributes where

  fromEnum CONN_ATTR_RO = #const CCI_CONN_ATTR_RO
  fromEnum CONN_ATTR_RU = #const CCI_CONN_ATTR_RU
  fromEnum CONN_ATTR_UU = #const CCI_CONN_ATTR_UU
  fromEnum CONN_ATTR_UU_MC_TX = #const CCI_CONN_ATTR_UU_MC_TX
  fromEnum CONN_ATTR_UU_MC_RX = #const CCI_CONN_ATTR_UU_MC_RX

  toEnum #{const CCI_CONN_ATTR_RO} = CONN_ATTR_RO
  toEnum #{const CCI_CONN_ATTR_RU} = CONN_ATTR_RU
  toEnum #{const CCI_CONN_ATTR_UU} = CONN_ATTR_UU
  toEnum #{const CCI_CONN_ATTR_UU_MC_TX} = CONN_ATTR_UU_MC_TX
  toEnum #{const CCI_CONN_ATTR_UU_MC_RX} = CONN_ATTR_UU_MC_RX
  toEnum i = error$ "ConnectionAttributes toEnum: unknown value: "++show i


------------------------------------------
--
-- Data transfers
--
------------------------------------------

-- $dt
-- CCI has two modes of communication: active messages (AM) and 
-- remote memory access (RMA).


---------
-- Active Messages
---------

-- $amsg 
-- Loosely based on Berkeley's Active Messages, CCI's messages are small 
-- (typically MTU sized) messages. Unlike Berkeley's AM, the message header 
-- does not include a handler address. Instead, the receiver's CCI library will 
-- generate a receive event that includes a pointer to the data within the CCI 
-- library's buffers. The application may inspect the data, copy it out if needed 
-- longer term, or even forward the data by passing it to the send function. 
-- By using events instead of handlers, the application does not block further 
-- communication progress.


-- | Send a short message.
--
-- A short message limited to the size of 'connectionMaxSendSize'.
--
-- If the application needs to send a message larger than
-- 'connectionMaxSendSize', the application is responsible for
-- segmenting and reassembly or it should use 'rmaWrite'.
--
-- When send returns, the provided ByteString is disposable. By
-- default, CCI will buffer the data internally.
--
-- The send will complete differently in reliable and unreliable
-- connections:
--
--  * Reliable: only when remote side ACKs complete delivery -- but not
--    necessary consumption (i.e., remote completion).
--
--  * Unreliable: when the buffer is re-usable (i.e., local completion).
--
-- Flags:
--
--  * If the 'SEND_BLOCKING' flag is specified, 'send' will also
--    block until the send completion has occurred. In this case, there
--    is no event returned for this send via 'getEvent', the send
--    completion status is returned via 'send'. A safe foreign call
--    is made when using this flag, if you intend to call this function
--    as blocking a lot, perhaps you should consider implementing the blocking 
--    behavior on the Haskell side.
--
--  * If the 'SEND_NO_COPY' is specified, the application is
--    indicating that it does not need the buffer back until the send
--    completion occurs (which is most useful when FLAG_BLOCKING is
--    not specified). The CCI implementation is therefore free to use
--    \"zero copy\" types of transmission with the buffer -- if it wants to.
--    Make sure to keep alive the memory of the ByteString message 
--    or it could be garbage collected before the send completes.
--
--  * 'SEND_SILENT' means that no completion will be generated for
--    non-FLAG_BLOCKING sends. For reliable ordered connections,
--    since completions are issued in order, the completion of any
--    non-SILENT send directly implies the completion of any previous
--    SILENT sends. For unordered connections, completion ordering is not
--    guaranteed -- it is not safe to assume that application protocol
--    semantics imply specific unordered SILENT send completions. The
--    only ways to know when unordered SILENT sends have completed (and
--    that the local send buffer is \"owned\" by the application again) is
--    either to close the connection or issue a non-SILENT send. The
--    completion of a non-SILENT send guarantees the completion of all
--    previous SILENT sends.
--
-- May throw 'ERR_DISCONNECTED', 'ERR_RNR', or driver-specific errors.
--
send :: Connection -> ByteString -> WordPtr -> [SEND_FLAG] -> IO ()
send (Connection pconn) msg pctx flags = unsafeUseAsCStringLen msg$ \(cmsg,clen) -> 
    let csend = if elem SEND_BLOCKING flags then safe_cci_send else cci_send
     in csend pconn cmsg (fromIntegral clen) (wordPtrToPtr pctx) (fromIntegral$ foldl (\f-> (f.|.) . fromEnum) (0::Int) flags)
           >>= cci_check_exception

foreign import ccall unsafe cci_send :: Ptr ConnectionV -> Ptr CChar -> Word32 -> Ptr () -> CInt -> IO CInt
foreign import ccall safe "cci_send" safe_cci_send :: Ptr ConnectionV -> Ptr CChar -> Word32 -> Ptr () -> CInt -> IO CInt


-- | Send a short vectored (gather) message.
--
-- Like 'send', 'sendv' sends a short message bound by
-- 'connectionMaxSendSize'. Instead of a single data buffer,
-- 'sendv' allows the application to gather a list of 
-- ByteStrings.
--
-- May throw driver-specific errors.
--
sendv :: Connection -> [ByteString] -> WordPtr -> [SEND_FLAG] -> IO ()
sendv (Connection pconn) msgs pctx flags = unsafeUseAsCStringLens msgs [] 0$ \cmsgs clen -> 
    let csendv = if elem SEND_BLOCKING flags then safe_cci_sendv else cci_sendv
     in allocaBytes (clen * #{size struct iovec})$ \piovecs -> do
          sequence_ (zipWith (write_iovec piovecs) [clen-1,clen-2..0] cmsgs) 
          csendv pconn piovecs (fromIntegral clen) (wordPtrToPtr pctx) (enumFlags flags)
             >>= cci_check_exception
  where
    unsafeUseAsCStringLens :: [ByteString] -> [CStringLen] -> Int -> ([CStringLen] -> Int -> IO a) -> IO a
    unsafeUseAsCStringLens (x:xs) acc len f
        | seq len True = unsafeUseAsCStringLen x$ \cx -> unsafeUseAsCStringLens xs (cx:acc) (len+1) f
    unsafeUseAsCStringLens _ acc len f = f acc len

    write_iovec piovecs offs (cmsg,clen) = do
        let p = plusPtr piovecs (offs * #{size struct iovec})
        #{poke struct iovec, iov_base} p cmsg
        #{poke struct iovec, iov_len} p clen


foreign import ccall unsafe cci_sendv :: Ptr ConnectionV -> Ptr () -> Word32 -> Ptr () -> CInt -> IO CInt
foreign import ccall safe "cci_sendv" safe_cci_sendv :: Ptr ConnectionV -> Ptr () -> Word32 -> Ptr () -> CInt -> IO CInt

enumFlags :: (Enum a, Num b) => [a] -> b
enumFlags = fromIntegral . foldl (\f-> (f.|.) . fromEnum) (0::Int)


-- | Flags for 'send' and 'sendv'. See 'send' for details.
data SEND_FLAG =
    SEND_BLOCKING 
  | SEND_NO_COPY  
  | SEND_SILENT 
 deriving (Eq, Show)

instance Enum SEND_FLAG where

  fromEnum SEND_BLOCKING = #const CCI_FLAG_BLOCKING
  fromEnum SEND_NO_COPY = #const CCI_FLAG_NO_COPY
  fromEnum SEND_SILENT = #const CCI_FLAG_SILENT

  toEnum #{const CCI_FLAG_BLOCKING} = SEND_BLOCKING
  toEnum #{const CCI_FLAG_NO_COPY} = SEND_NO_COPY
  toEnum #{const CCI_FLAG_SILENT} = SEND_SILENT
  toEnum i = error$ "SEND_FLAG toEnum: unknown value: "++show i




---------
-- RMA
---------

-- $rma
-- When an application needs to move bulk data, CCI provides RMA. To 
-- use RMA, the application will explicitly register memory with CCI 
-- and receive a handle. The application can pass the handle to a peer 
-- using an active message and then perform a RMA Read or Write. The 
-- RMA may also include a Fence. RMA requires a reliable connection 
-- (ordered or unordered). An RMA may optionally include a remote 
-- completion message that will be delivered to the peer after the RMA 
-- completes. The completion message may be as large as a full active 
-- message.



-- | Registers memory for RMA operations on the connections of an endpoint.
--
-- The intent is that this function is invoked frequently -- \"just
-- register everything\" before invoking RMA operations.
--
-- In the best case, the implementation is cheap/fast enough that the
-- invocation time doesn't noticeably affect performance (e.g., MX and
-- PSM). If the implementation is slow (e.g., IB/iWARP), this function
-- should probably have a registration cache so that at least repeated
-- registrations are fast.
--
-- It is allowable to have overlapping registerations.
--
-- May throw:
--
--  * 'EINVAL' if the connection is unreliable or the register buffer has length 0.
--
--  * driver-specific errors.
--
rmaEndpointRegister :: Endpoint -> CStringLen -> IO RMALocalHandle
rmaEndpointRegister (Endpoint pend) (cbuf,clen) = alloca$ \p ->
    cci_rma_register pend nullPtr cbuf (fromIntegral clen) p
      >>= cci_check_exception >> peek p

foreign import ccall unsafe cci_rma_register :: Ptr EndpointV -> Ptr ConnectionV 
                                             -> Ptr CChar -> Word64 -> Ptr RMALocalHandle -> IO CInt


-- | Like 'rmaEndpointRegister' but registers memory for RMA operations on a specific connection instead.
rmaConnectionRegister :: Connection -> CStringLen -> IO RMALocalHandle
rmaConnectionRegister (Connection pconn) (cbuf,clen) = alloca$ \p ->
    cci_rma_register nullPtr pconn cbuf (fromIntegral clen) p
      >>= cci_check_exception >> peek p


-- | Deregisters memory.
--
-- If an RMA is in progress that uses this handle, the RMA may abort or
-- the deregisteration may fail.
--
-- May throw driver-specific errors.
--
rmaDeregister :: RMALocalHandle -> IO ()
rmaDeregister (RMALocalHandle w64) = cci_rma_deregister w64 >>= cci_check_exception

foreign import ccall unsafe cci_rma_deregister :: Word64 -> IO CInt


-- | Wraps an IO operation with calls to 'rmaEndpointRegister' and 'rmaDeregister'.
--
-- This function makes sure to call 'rmaDeregister' even in the presence of errors.
--
withEndpointRMALocalHandle :: Endpoint -> CStringLen -> (RMALocalHandle -> IO a) -> IO a
withEndpointRMALocalHandle e cs = bracket (rmaEndpointRegister e cs) rmaDeregister

-- | Like 'withEndpointRMAHandle' but uses 'rmaConnectionRegister' instead of 'rmaEndpointRegister'.
withConnectionRMALocalHandle :: Connection -> CStringLen -> (RMALocalHandle -> IO a) -> IO a
withConnectionRMALocalHandle c cs = bracket (rmaConnectionRegister c cs) rmaDeregister


-- | Transfers data in remote memory to local memory.
--
-- An RMA operation is a data transfer between local and remote buffers.
-- In order to obtain a RMAHandle to local buffers, you should use one of 
-- 'rmaEndpointRegister' or 'rmaConnectionRegister'. To obtain a RMAHandle
-- to a remote buffer, the handle should be transmitted
--
-- Adding the 'RMA_FENCE' flag ensures all previous operations are guaranteed to complete
-- remotely prior to this operation and all subsequent operations. Remote
-- completion does not imply a remote completion event, merely a successful
-- RMA operation.
--
-- Optionally, sends a remote completion event to the target. If msg_ptr
-- and msg_len are provided, send a completion event to the target after
-- the RMA has completed. It is guaranteed to arrive after the RMA operation
-- has finished.
--
-- CCI makes no guarantees about the data delivery within the RMA operation
-- (e.g., no last-byte-written-last).
--
-- Only a local completion 'EvSend' will be generated.
--
-- The following flags might be specified:
--
--  * 'RMA_BLOCKING': Blocking call (see 'send' for details).
--
--  * 'RMA_FENCE': All previous operations are guaranteed to
--    complete remotely prior to this operation
--    and all subsequent operations.
--
--  * 'RMA_SILENT': Generates no local completion event (see 'send'
--    for details).
-- 
-- May throw:
--
--  * 'EINVAL' if the connection is unreliable or the data length is 0.
--
--  * driver-specific errors.
--
rmaRead :: Connection          -- ^ Connection used for the RMA transfer.
        -> Maybe ByteString    -- ^ If @Just bs@, sends @bs@ as completion event to the peer. 
        -> RMALocalHandle      -- ^ Handle to the transfer destination.
        -> Word64              -- ^ Offset inside the destination buffer.
        -> RMARemoteHandle     -- ^ Handle to the transfer source.
        -> Word64              -- ^ Offset inside the source buffer.
        -> Word64              -- ^ Length of the data to transfer.
        -> WordPtr             -- ^ Context to deliver in the local 'EvSend' event.
        -> [RMA_FLAG]          -- ^ Flags specifying the transfer.
        -> IO ()
rmaRead (Connection pconn) mb (RMALocalHandle lh) lo (RMARemoteHandle rh) ro dlen pctx flags = 
  let crma (cb,clen) = cci_rma pconn cb (fromIntegral clen) lh lo rh ro dlen 
                               (wordPtrToPtr pctx) (#{const CCI_FLAG_READ} .|. enumFlags flags) 
                           >>= cci_check_exception
   in case mb of
       Just b -> unsafeUseAsCStringLen b$ crma
       Nothing -> crma (nullPtr,0::Int)

foreign import ccall unsafe cci_rma :: Ptr ConnectionV -> Ptr CChar -> Word32 
                                    -> Word64 -> Word64 -> Word64 -> Word64 -> Word64 
                                    -> Ptr () -> CInt -> IO CInt


-- | Transfers data in local memory to remote memory.
--
-- Flags are set the same than for 'rmaRead'.
--
rmaWrite :: Connection          -- ^ Connection used for the RMA transfer.
         -> Maybe ByteString    -- ^ If @Just bs@, sends @bs@ as completion event to the peer. 
         -> RMARemoteHandle     -- ^ Handle to the transfer destination.
         -> Word64              -- ^ Offset inside the destination buffer.
         -> RMALocalHandle      -- ^ Handle to the transfer source.
         -> Word64              -- ^ Offset inside the source buffer.
         -> Word64              -- ^ Length of the data to transfer.
         -> WordPtr             -- ^ Context to deliver in the local 'EvSend' event.
         -> [RMA_FLAG]          -- ^ Flags specifying the transfer.
         -> IO ()
rmaWrite (Connection pconn) mb (RMARemoteHandle rh) ro (RMALocalHandle lh) lo dlen pctx flags = 
  let crma (cb,clen) = cci_rma pconn cb (fromIntegral clen) lh lo rh ro dlen 
                               (wordPtrToPtr pctx) (#{const CCI_FLAG_WRITE} .|. enumFlags flags) 
                           >>= cci_check_exception
   in case mb of
       Just b -> unsafeUseAsCStringLen b$ crma
       Nothing -> crma (nullPtr,0::Int)


-- | Flags for 'rmaRead' and 'rmaWrite'.
data RMA_FLAG =
    RMA_BLOCKING 
  | RMA_NO_COPY  
  | RMA_SILENT 
  | RMA_FENCE


instance Enum RMA_FLAG where

  fromEnum RMA_BLOCKING = #const CCI_FLAG_BLOCKING
  fromEnum RMA_NO_COPY = #const CCI_FLAG_NO_COPY
  fromEnum RMA_SILENT = #const CCI_FLAG_SILENT
  fromEnum RMA_FENCE = #const CCI_FLAG_FENCE

  toEnum #{const CCI_FLAG_BLOCKING} = RMA_BLOCKING
  toEnum #{const CCI_FLAG_NO_COPY} = RMA_NO_COPY
  toEnum #{const CCI_FLAG_SILENT} = RMA_SILENT
  toEnum #{const CCI_FLAG_FENCE} = RMA_FENCE
  toEnum i = error$ "RMA_FLAG toEnum: unknown value: "++show i



-- | RMA local handles have an associated buffer in local memory
-- which is read or written during RMA operations.
--
newtype RMALocalHandle = RMALocalHandle Word64
  deriving Storable

-- | RMA remote handles have an associated buffer in a remote location
-- which is read or written during RMA operations.
--
newtype RMARemoteHandle = RMARemoteHandle Word64


-- | Gets a ByteString representation of the handle which can be sent to a peer.
rmaHandle2ByteString :: RMALocalHandle -> ByteString
rmaHandle2ByteString (RMALocalHandle w64) = pack [ fromIntegral ((w64 `shiftR` i) .&. 255)  | i<-[56,48..0] ]

-- | Creates a remote handle from a ByteString representation of a remote
-- handle (has to have arrived through an active message).
createRMARemoteHandle :: ByteString -> Maybe RMARemoteHandle
createRMARemoteHandle b = case B.length b of
                            8 -> Just$ RMARemoteHandle$ toWord64$ unpack b
                            _ -> Nothing
  where 
    toWord64 :: [Word8] -> Word64
    toWord64 = foldl (\w64 w8 -> (w64 `shiftL` 8) .|. fromIntegral w8) 0



------------------------------------------
--
-- Event handling
--
------------------------------------------


-- | Get the next available CCI event.
--
-- This function never blocks; it polls instantly to see if there is
-- any pending event of any type (send completion, receive, or other
-- events -- errors, incoming connection requests, etc.). If you want to
-- block, use the OS handle to use your OS's native blocking mechanism
-- (e.g., select/poll on the POSIX fd). This also allows the app to
-- busy poll for a while and then OS block if nothing interesting is
-- happening. The default OS handle returned when creating the
-- endpoint will return the equivalent of a POLL_IN when any event is
-- available.
--
-- This function borrows the buffer associated with the event; it must
-- be explicitly returned later via returnEvent.
--
-- May throw driver-specific errors.
--
-- The garbage collector will call 'returnEvent' if there are no
-- references to the returned event and memory is claimed.
--
getEvent :: Endpoint -> IO (Maybe Event)
getEvent (Endpoint pend) = alloca$ \pev -> do
    st <- cci_get_event pend pev
    case st of
      #{const CCI_EAGAIN} -> return Nothing
      _ -> cci_check_exception st >> fmap Just (peek pev)


foreign import ccall unsafe cci_get_event :: Ptr EndpointV -> Ptr Event -> IO CInt


-- | This function gives back to the CCI implementation the 
-- buffer associated with an event that was previously obtained 
-- via 'getEvent'. The data buffer associated with the event will 
-- immediately become stale to the application.
--
-- Events may be returned in any order; they do not need to be returned
-- in the same order that 'getEvent' issued them. All events
-- must be returned, even send completions and \"other\" events, not
-- just receive events. However, it is possible (likely) that
-- returning send completion and \"other\" events will be no-ops.
--
-- May throw driver-specific errors.
--
returnEvent :: Event -> IO ()
returnEvent (Event pev) = cci_return_event pev >>= cci_check_exception

foreign import ccall unsafe cci_return_event :: Ptr EventV -> IO CInt


-- | Wraps an IO action with calls to 'getEvent', 'getEventData' and 'returnEvent'.
--
-- Makes sure the event is returned to the CCI implementation
-- even in the presence of errors.
--
-- The callback IO action is performed with asynchronous exceptions blocked. Otherwise
-- a message taken out of the queue would be lost before it can be adequately processed.
-- If this does not fit your needs consider using one of 'Control.Exception.bracket' or
-- 'Control.Exception.mask' in combination with 'getEvent'.
--
tryWithEventData :: BufferHandler buffer => 
   Endpoint   -- ^ Endpoint on which to listen for events.
   -> IO a    -- ^ Action to perform in case no event is available.
   -> (EventData buffer -> IO a) -- ^ Callback for the case when an event is available.
   -> IO a  -- ^ Yields the callback or the no-event action result.
tryWithEventData endp noEvent f = mask_$ do
    mev <- getEvent endp 
    case mev of
      Nothing -> noEvent
      Just ev -> do
          evd <- getEventData ev
          r <- f evd `onException` returnEvent ev
          returnEvent ev
          return r

-- | Like 'tryWithEventData' but blocks if no events are available.
withEventData :: BufferHandler buffer => 
   Endpoint   -- ^ Endpoint on which to listen for events.
   -> Fd      -- ^ OS handle to block onto.
   -> (EventData buffer -> IO a) 
   -> IO a  -- ^ Yields the callback result.
withEventData endp fd f = go
  where
    go = tryWithEventData endp (threadWaitRead fd >> go) f


-- | Like 'tryWithEventData' but loops polling until events are available.
pollWithEventData :: BufferHandler buffer => 
   Endpoint   -- ^ Endpoint on which to listen for events.
   -> (EventData buffer -> IO a) 
   -> IO a  -- ^ Yields the callback result.
pollWithEventData endp f = go
  where
    go = tryWithEventData endp go f


-- | Event representation
newtype Event = Event (Ptr EventV)
  deriving (Storable, Show)

data EventV


-- | Retrieves the public data contained in an event.
getEventData :: BufferHandler buffer => Event -> IO (EventData buffer)
getEventData ev@(Event pev) = do
    st <- #{peek cci_event_t, type} pev
    case st::CInt of
      #{const CCI_EVENT_SEND} -> 
          liftM3 EvSend (#{peek cci_event_send_t, context} pev)
                        (fmap ctoEnum$  #{peek cci_event_send_t, status} pev)
                        (#{peek cci_event_send_t, connection} pev)
      #{const CCI_EVENT_RECV} -> 
          liftM2 EvRecv (join$ liftM2 cmkBuffer
                                      (#{peek cci_event_recv_t, ptr} pev) 
                                      (#{peek cci_event_recv_t, len} pev))
                        (#{peek cci_event_recv_t, connection} pev)
      #{const CCI_EVENT_CONNECT_ACCEPTED} -> 
          liftM2 EvConnectAccepted (fmap ptrToWordPtr$ #{peek cci_event_connect_accepted_t, context} pev) 
                                   (#{peek cci_event_recv_t, connection} pev)
      #{const CCI_EVENT_CONNECT_TIMEDOUT} -> 
          fmap EvConnectTimedOut (#{peek cci_event_connect_timedout_t, context} pev) 
      #{const CCI_EVENT_CONNECT_REJECTED} -> 
          fmap EvConnectRejected (#{peek cci_event_connect_rejected_t, context} pev) 
      #{const CCI_EVENT_CONNECT_REQUEST} -> 
          liftM2 (EvConnectRequest ev) (join$ liftM2 cmkBuffer
                                              (#{peek cci_event_connect_request_t, data_ptr} pev) 
                                              (#{peek cci_event_connect_request_t, data_len} pev))
                                       (fmap ctoEnum$ #{peek cci_event_connect_request_t, attribute} pev)
      #{const CCI_EVENT_KEEPALIVE_TIMEDOUT} -> 
          fmap EvKeepAliveTimedOut (#{peek cci_event_keepalive_timedout_t, connection} pev) 
      #{const CCI_EVENT_ENDPOINT_DEVICE_FAILED} -> 
          fmap EvEndpointDeviceFailed (#{peek cci_event_endpoint_device_failed_t, endpoint} pev) 
          
      _ -> error$ "getEventData: unexpected event type: "++show st
  where
    cmkBuffer p len = mkBuffer (p,fromIntegral (len::CInt))
    ctoEnum :: Enum a => CInt -> a
    ctoEnum = toEnum . fromIntegral


-- | Determines how a buffer is to be treated.
--
-- Instances of this class can copy the data into a Haskell value
-- or they can keep a reference to the data without copying it.
class BufferHandler buffer where
  -- | Creates a buffer from a 'CStringLen'
  mkBuffer :: CStringLen -> IO buffer

-- | Copies provided data into a ByteString.
instance BufferHandler ByteString where
  mkBuffer = packCStringLen

-- | A ByteString which is valid as long as the
-- underlying buffer is not freed.
--
-- The underlying buffer is not managed by the garbage collector.
newtype VolatilByteString = VolatilB ByteString

-- | Creates a ByteString which points to the provided data.
--
-- The ByteString is usable as long as the buffer containing the
-- provided data is valid.
instance BufferHandler VolatilByteString where
  mkBuffer = fmap VolatilB . unsafePackCStringLen

-- | Passes the buffer containing the provided data unmodified.
instance BufferHandler CStringLen where
  mkBuffer = return


-- | Representation of data contained in events.
data EventData buffer =

    -- | A 'send' or 'rmaRead' has completed.
    --
    -- On a reliable connection, a sender will generally complete a send
    -- when the receiver replies for that message. Additionally, an error
    -- status may be returned ('ERR_DISCONNECTED', 'ERR_RNR').
    --
    -- On an unreliable connection, a sender will return 'SUCCESS' upon
    -- local completion (i.e., the message has been queued up to some lower
    -- layer -- there is no guarantee that it is \"on the wire\", etc.).
    -- Other send statuses will only be returned for local errors.
    --
    -- Contains the context provided to 'send', the result of the 
    -- send and the connection.
    --
    EvSend WordPtr Status Connection

    -- | An active message has been received.
    --
    -- A completion event is returned for each message received.
    --
    -- Contains the transmitted data which is valid as long as the event
    -- is not returned ('returnEvent').
    -- 
  | EvRecv buffer Connection

    -- | A new outgoing connection was successfully accepted at the
    -- peer; a connection is now available for data transfer.
    --
    -- Contains the context given to 'connect'.
    --
  | EvConnectAccepted WordPtr Connection

    -- | A new outgoing connection did not complete the accept/connect
    -- handshake with the peer in the time specified as argument to
    -- 'connect'. CCI has therefore given up attempting to continue to 
    -- create this connection.
    --
    -- Contains the context given to 'connect'.
    --
  | EvConnectTimedOut WordPtr

    -- | A new outgoing connection was rejected by the server.
    --
    -- Contains the context given to 'connect'.
    --
  | EvConnectRejected WordPtr

    -- | An incoming connection request from a client.
    --
    -- Contains the data transmitted with the request, the
    -- connection attributes and a reference to the event
    -- for convenience to call 'accept' and 'reject'.
    --
  | EvConnectRequest Event buffer ConnectionAttributes

    -- | The keepalive timeout has expired, i.e. no data from the
    -- peer has been received during that time period
    -- (see'OPT_ENDPT_KEEPALIVE_TIMEOUT' for more details).
    --
  | EvKeepAliveTimedOut Connection

    -- | A device on this endpoint has failed.
    --
    -- Contains the endpoint on the device that failed.
  | EvEndpointDeviceFailed Endpoint

 deriving Show




------------------------------------------
--
-- Setting options
--
------------------------------------------


-- | Sets a connection option value.
--
-- May throw:
--
--  * 'ERR_NOT_IMPLEMENTED' Not supported by this driver.
--
--  * driver-specific errors.
--
setConnectionOpt :: Connection -> ConnectionOption -> Word32 -> IO ()
setConnectionOpt (Connection pconn) co v = allocaBytes #{size cci_opt_handle_t}$ \ph ->
    alloca$ \pv -> do
      poke pv v
      #{poke cci_opt_handle_t, connection} ph pconn
      cci_set_opt ph #{const CCI_OPT_LEVEL_CONNECTION} (fromIntegral$ fromEnum co) pv (fromIntegral$ sizeOf v)
        >>= cci_check_exception

foreign import ccall unsafe cci_set_opt :: Ptr () -> CInt -> CInt -> Ptr Word32 -> CInt -> IO CInt


-- | Sets an endpoint option value
--
-- May throw:
--
--  * 'ERR_NOT_IMPLEMENTED' Not supported by this driver.
--
--  * driver-specific errors.
--
setEndpointOpt :: Endpoint -> EndpointOption -> Word32 -> IO ()
setEndpointOpt (Endpoint pend) eo v = allocaBytes #{size cci_opt_handle_t}$ \ph ->
    alloca$ \pv -> do
      poke pv v
      #{poke cci_opt_handle_t, endpoint} ph pend
      cci_set_opt ph #{const CCI_OPT_LEVEL_ENDPOINT} (fromIntegral$ fromEnum eo) pv (fromIntegral$ sizeOf v)
        >>= cci_check_exception


-- | Retrieves a connection option value.
--
-- May throw:
--
--  * 'ERR_NOT_IMPLEMENTED' Not supported by this driver.
--
--  * driver-specific errors.
--
getConnectionOpt :: Connection -> ConnectionOption -> IO Word32
getConnectionOpt (Connection pconn) co = allocaBytes #{size cci_opt_handle_t}$ \ph ->
    alloca$ \pv -> alloca$ \pl -> do
      #{poke cci_opt_handle_t, connection} ph pconn
      cci_get_opt ph #{const CCI_OPT_LEVEL_CONNECTION} (fromIntegral$ fromEnum co) pv pl
        >>= cci_check_exception
      peek pv >>= peek

foreign import ccall unsafe cci_get_opt :: Ptr () -> CInt -> CInt -> Ptr (Ptr Word32) -> Ptr CInt -> IO CInt


-- | Retrieves an endpoint option value.
--
-- May throw:
--
--  * 'ERR_NOT_IMPLEMENTED' Not supported by this driver.
-- 
--  * driver-specific errors.
--
getEndpointOpt :: Endpoint -> EndpointOption -> IO Word32
getEndpointOpt (Endpoint pend) eo = allocaBytes #{size cci_opt_handle_t}$ \ph ->
    alloca$ \pv -> alloca$ \pl -> do
      #{poke cci_opt_handle_t, endpoint} ph pend
      cci_get_opt ph #{const CCI_OPT_LEVEL_ENDPOINT} (fromIntegral$ fromEnum eo) pv pl
        >>= cci_check_exception
      peek pv >>= peek



-- | Endpoint options
data EndpointOption =
    -- | Default send timeout for all new connections.
    OPT_ENDPT_SEND_TIMEOUT 

    -- | How many receiver buffers on the endpoint. It is the max
    -- number of messages the CCI layer can receive without dropping.
  | OPT_ENDPT_RECV_BUF_COUNT

    -- | How many send buffers on the endpoint. It is the max number of
    -- pending messages the CCI layer can buffer before failing or
    -- blocking (depending on reliability mode).
  | OPT_ENDPT_SEND_BUF_COUNT

  -- | The \"keepalive\" timeout is to prevent a client from connecting
  -- to a server and then the client disappears without the server
  -- noticing. If the server never sends anything on the connection,
  -- it'll never realize that the client is gone, but the connection
  -- is still consuming resources. But note that keepalive timers
  -- apply to both clients and servers.
  --
  -- The keepalive timeout is expressed in microseconds. If the
  -- keepalive timeout value is set:
  --
  --  * If no traffic at all is received on a connection within the
  --    keepalive timeout, the EVENT_KEEPALIVE_TIMEOUT event is
  --    raised on that connection.
  --
  --  * The CCI implementation will automatically send control
  --    hearbeats across an inactive (but still alive) connection to
  --    reset the peer's keepalive timer before it times out.
  -- 
  -- If a keepalive event is raised, the keepalive timeout is set to
  -- 0 (i.e., it must be \"re-armed\" before it will timeout again),
  -- but the connection is *not* disconnected. Recovery decisions
  -- are up to the application; it may choose to 'disconnect' the
  -- connection, re-arm the keepalive timeout, etc.
  | OPT_ENDPT_KEEPALIVE_TIMEOUT


instance Enum EndpointOption where

  toEnum #{const CCI_OPT_ENDPT_SEND_TIMEOUT} = OPT_ENDPT_SEND_TIMEOUT
  toEnum #{const CCI_OPT_ENDPT_RECV_BUF_COUNT} = OPT_ENDPT_RECV_BUF_COUNT
  toEnum #{const CCI_OPT_ENDPT_SEND_BUF_COUNT} = OPT_ENDPT_SEND_BUF_COUNT
  toEnum #{const CCI_OPT_ENDPT_KEEPALIVE_TIMEOUT} = OPT_ENDPT_KEEPALIVE_TIMEOUT
  toEnum v = error$ "EndpointOption toEnum: unknown option value: "++show v

  fromEnum OPT_ENDPT_SEND_TIMEOUT = #const CCI_OPT_ENDPT_SEND_TIMEOUT
  fromEnum OPT_ENDPT_RECV_BUF_COUNT = #const CCI_OPT_ENDPT_RECV_BUF_COUNT
  fromEnum OPT_ENDPT_SEND_BUF_COUNT = #const CCI_OPT_ENDPT_SEND_BUF_COUNT
  fromEnum OPT_ENDPT_KEEPALIVE_TIMEOUT = #const CCI_OPT_ENDPT_KEEPALIVE_TIMEOUT
 


-- | Connection options
data ConnectionOption =
    -- | Reliable send timeout in microseconds.
    OPT_CONN_SEND_TIMEOUT 

instance Enum ConnectionOption where

  toEnum #{const CCI_OPT_CONN_SEND_TIMEOUT} = OPT_CONN_SEND_TIMEOUT
  toEnum v = error$ "ConnectionOption toEnum: unknown option value: "++show v

  fromEnum OPT_CONN_SEND_TIMEOUT = #const CCI_OPT_CONN_SEND_TIMEOUT
  


------------------------------------------
--
-- Error handling
--
------------------------------------------


-- | Returns a human readable description of a 'Status' value.
strError :: Status -> IO String
strError st = do
     cs <- cci_strerror$ fromIntegral$ fromEnum st
     if nullPtr == cs then return ""
       else peekCString cs

foreign import ccall unsafe cci_strerror :: CInt -> IO CString


-- | Type of exceptions that can arise when calling cci functions.
data CCIException = CCIException Status
  deriving (Show,Typeable)

instance Exception CCIException


-- Error codes resulting from CCI operations.
data Status =
    SUCCESS -- ^ Returned from most functions when they succeed.
  | ERROR -- ^ Generic error

  -- Send completion status codes

    -- | For both reliable and unreliable sends, this error code means
    -- that 'disconnect' has been invoked on the send side (in
    -- which case this is an application error), or the receiver
    -- replied that the receiver invoked 'disconnect'.
  | ERR_DISCONNECTED

    -- | For a reliable send, this error code means that a receiver
    -- is reachable, the connection is connected but the receiver
    -- could not receive the incoming message during the timeout
    -- period. If a receiver cannot receive an incoming message for
    -- transient reasons (most likely out of resources), it returns
    -- an Receiver-Not-Ready NACK and drops the message. The sender
    -- keeps retrying to send the message until the timeout expires,
    --
    -- If the timeout expires and the last control message received
    -- from the receiver was an RNR NACK, then this message is
    -- completed with the RNR status. If the connection is both
    -- reliable and ordered, then all successive sends are also
    -- completed in the order in which they were issued with the RNR
    -- status.
    --
    -- This error code will not be returned for unreliable sends.
  | ERR_RNR
  | ERR_DEVICE_DEAD -- ^ The local device is gone, not coming back 

    -- Error returned from remote peer indicating that the address was
    -- either invalid or unable to be used for access / permissions
    -- reasons.
  | ERR_RMA_HANDLE

    -- | Error returned from remote peer indicating that it does not support
    -- the operation that was requested.
  | ERR_RMA_OP
  | ERR_NOT_IMPLEMENTED -- ^ Not yet implemented
  | ERR_NOT_FOUND -- ^ Not found

  -- Errno.h error codes

  | EINVAL -- ^ Invalid parameter passed to CCI function call 
  
   -- | For a reliable send, this error code means that the sender did
   -- not get anything back from the receiver within a timeout (no
   -- ACK, no NACK, etc.). It is unknown whether the receiver
   -- actually received the message or not.
   --
   -- This error code won't occur for unreliable sends.
  | ETIMEDOUT
  | ENOMEM    -- ^ No more memory
  | ENODEV    -- ^ No device available
  | EBUSY     -- ^ Resource busy (e.g. port in use)
  | ERANGE    -- ^ Value out of range (e.g. no port available)
  | EAGAIN    -- ^ Resource temporarily unavailable 
  | ENOBUFS   -- ^ The output queue for a network interface is full
  | EMSGSIZE  -- ^ Message too long 
  | ENOMSG    -- ^ No message of desired type
  | EADDRNOTAVAIL -- ^ Address not available
  | OTHER Int
 deriving (Show,Read,Eq)


cci_check_exception :: CInt -> IO ()
cci_check_exception #{const CCI_SUCCESS} = return ()
cci_check_exception c = throwIO$ CCIException$ toEnum$ fromIntegral c


instance Enum Status where

 fromEnum c = 
   case c of
     SUCCESS             -> #{const CCI_SUCCESS}
     ERROR               -> #{const CCI_ERROR}
     ERR_DISCONNECTED    -> #{const CCI_ERR_DISCONNECTED}
     ERR_RNR             -> #{const CCI_ERR_RNR}
     ERR_DEVICE_DEAD     -> #{const CCI_ERR_DEVICE_DEAD}
     ERR_RMA_HANDLE      -> #{const CCI_ERR_RMA_HANDLE}
     ERR_RMA_OP          -> #{const CCI_ERR_RMA_OP}
     ERR_NOT_IMPLEMENTED -> #{const CCI_ERR_NOT_IMPLEMENTED}
     ERR_NOT_FOUND       -> #{const CCI_ERR_NOT_FOUND}
     EINVAL              -> #{const CCI_EINVAL}
     ETIMEDOUT           -> #{const CCI_ETIMEDOUT}
     ENOMEM              -> #{const CCI_ENOMEM}
     ENODEV              -> #{const CCI_ENODEV}
     EBUSY               -> #{const CCI_EBUSY}
     ERANGE              -> #{const CCI_ERANGE}
     EAGAIN              -> #{const CCI_EAGAIN}
     ENOBUFS             -> #{const CCI_ENOBUFS}
     EMSGSIZE            -> #{const CCI_EMSGSIZE}
     ENOMSG              -> #{const CCI_ENOMSG}
     EADDRNOTAVAIL       -> #{const CCI_EADDRNOTAVAIL}
     OTHER co            -> co
 
 toEnum c = 
   case c of
     #{const CCI_SUCCESS}             -> SUCCESS
     #{const CCI_ERROR}               -> ERROR
     #{const CCI_ERR_DISCONNECTED}    -> ERR_DISCONNECTED
     #{const CCI_ERR_RNR}             -> ERR_RNR
     #{const CCI_ERR_DEVICE_DEAD}     -> ERR_DEVICE_DEAD
     #{const CCI_ERR_RMA_HANDLE}      -> ERR_RMA_HANDLE
     #{const CCI_ERR_RMA_OP}          -> ERR_RMA_OP
     #{const CCI_ERR_NOT_IMPLEMENTED} -> ERR_NOT_IMPLEMENTED
     #{const CCI_ERR_NOT_FOUND}       -> ERR_NOT_FOUND
     #{const CCI_EINVAL}              -> EINVAL
     #{const CCI_ETIMEDOUT}           -> ETIMEDOUT
     #{const CCI_ENOMEM}              -> ENOMEM
     #{const CCI_ENODEV}              -> ENODEV
     #{const CCI_EBUSY}               -> EBUSY
     #{const CCI_ERANGE}              -> ERANGE
     #{const CCI_EAGAIN}              -> EAGAIN
     #{const CCI_ENOBUFS}             -> ENOBUFS
     #{const CCI_EMSGSIZE}            -> EMSGSIZE
     #{const CCI_ENOMSG}              -> ENOMSG
     #{const CCI_EADDRNOTAVAIL}       -> EADDRNOTAVAIL
     _ -> OTHER c
    


