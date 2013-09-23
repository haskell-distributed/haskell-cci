--
-- Copyright (C) 2012 Parallel Scientific. All rights reserved.
--
-- See the accompanying COPYING file for license information.
--

{-# LANGUAGE DeriveDataTypeable         #-} 
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GADTs                      #-} 

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__);}, y__)

-- | Haskell bindings for CCI. 
--
-- See <http://www.olcf.ornl.gov/center-projects/common-communication-interface> .
-- Most of the comments in this module has been taken and reedited from there.
--
module Network.CCI
  ( initCCI
  , finalizeCCI
  , withCCI
  -- * Devices
  , Device
  , getDeviceInfo
  , DeviceInfo(..)
  , getDevices
  -- * Endpoints
  , Endpoint
  , createBlockingEndpoint
  , createPollingEndpoint
  , destroyEndpoint
  , withBlockingEndpoint
  , withPollingEndpoint
  -- * Connections
  , Connection
  , connMaxSendSize
  , connEndpoint
  , connAttributes
  , connContext
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
  , sendBlocking
  , sendSilent
  , sendNoCopy
  , sendNoCopySilent
  , sendv
  , sendvBlocking
  , sendvSilent
  , sendvNoCopy
  , sendvNoCopySilent
  -- ** RMA
  -- $rma
  , rmaRegister
  , rmaDeregister 
  , withRMALocalHandle 
  , RMA_MODE(..)
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
  , EventBytes
  , packEventBytes
  , unsafePackEventBytes
  , EventData(..)
  -- * Endpoint options
  , getEndpt_SendTimeout
  , setEndpt_SendTimeout
  , getEndpt_SendBufCount
  , setEndpt_SendBufCount
  , getEndpt_RecvBufCount
  , setEndpt_RecvBufCount
  , getEndpt_KeepAliveTimeout
  , setEndpt_KeepAliveTimeout
  , getEndpt_RMAAlign
  , getEndpt_URI
  , RMAAlignments(..)
  -- * Connection options
  , getConn_SendTimeout
  , setConn_SendTimeout
  -- * Error handling
  , strError 
  , CCIException(..)
  , Status(..)
  ) where

import Control.Concurrent     ( threadWaitRead )
import Control.Exception      ( Exception, throwIO, bracket, bracket_, onException, mask_ )
import Control.Monad          ( liftM2, liftM3, join )
import Data.Bits              ( (.|.) )
import Data.ByteString as B   ( ByteString, packCStringLen, length )
import Data.ByteString.Unsafe ( unsafePackCStringLen, unsafeUseAsCStringLen, unsafeUseAsCString )
import Data.Dynamic           ( Typeable )
import Data.Word              ( Word32, Word64 )
import Foreign.C.Types        ( CInt(..), CChar )
import Foreign.C.String       ( CString, peekCString, CStringLen, withCString )
import Foreign.Ptr            ( Ptr, nullPtr, WordPtr, wordPtrToPtr, plusPtr, ptrToWordPtr, castPtr )
import Foreign.Marshal.Alloc  ( alloca, allocaBytesAligned )
import Foreign.Storable       ( Storable(..) )
import System.Posix.Types     ( Fd(Fd) )
import System.IO.Unsafe       ( unsafePerformIO )


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


-- | This is the last CCI function that must be called; no other
--   CCI functions can be invoked after this function.
--
--  Returns 'SUCCESS' iff CCI has been properly finalized.
--
--  If initCCI was invoked multiple times, finalizeCCI should be
--  called as many times, and only the last one will not be a no-op.
--
finalizeCCI :: IO ()
finalizeCCI = cci_finalize >>= cci_check_exception

foreign import ccall unsafe cci_finalize :: IO CInt


-- | Wraps an IO action between calls to 'initCCI' and 'finalizeCCI'.
withCCI :: IO a -> IO a
withCCI = bracket_ initCCI finalizeCCI


------------------------------------------
-- Devices
------------------------------------------

-- | A device represents a network interface card (NIC) or host channel adapter 
-- (HCA) that connects the host and network. A device may provide multiple 
-- endpoints (typically one per core).
--
data Device = Device 
    { devPtr :: Ptr Device
    , getDeviceInfo :: DeviceInfo -- ^ Device information 
    }

-- | Device information.
data DeviceInfo = DeviceInfo
    { devName :: String  -- ^ Name of the device from the config file, e.g. @"bob0"@

    , devTransport :: String -- ^ Name of the device driver, e.g., @"sock"@ or @"verbs"@

    , devUp :: Bool -- ^ Is this device actually up and running?

    , devInfo :: String -- ^ Human readable description string (to include newlines); should
	                    -- contain debugging info, probably the network address of the
                        -- device at a bare minimum.

    , devConfArgs :: [String] -- ^ @"key=value"@ strings from the config file for this device.

    , devMaxSendSize :: Word32 -- ^ Maximum send size supported by the device
    , devRate :: Word64 -- ^ Data rate per specification: data bits per second (not the
                        -- signaling rate). @0@ if unknown.

    , devPCI :: (Word32,Word32,Word32,Word32) -- ^ The PCI ID of this device as reported by the OS/hardware.  All
	                                          -- values will be @0xFFFF@ for non-PCI devices (e.g. shared memory)
		                                      -- (domain, bus, dev, func)
    }
  deriving Show


-- | Returns a list of \"up\" devices.
--
-- The order of devices returned corresponds to the priority fields in
-- the devices. If two devices share the same priority, their
-- ordering in the return array is arbitrary.
--
getDevices :: IO [Device]
getDevices = do
    pdev <- alloca$ \ppdev -> cci_get_devices ppdev >>= cci_check_exception >> peek ppdev
    devices <- peekNullTerminated pdev 0
    mapM mkDevice devices
  where
    peekNullTerminated :: Ptr (Ptr a) -> Int -> IO [Ptr a]
    peekNullTerminated p i = do
      d <- peekElemOff p i
      if d == nullPtr then return []
        else fmap (d:)$ peekNullTerminated p (i+1) 

    mkDevice :: Ptr Device -> IO Device
    mkDevice p = do
        name <- (#{peek cci_device_t, name} p) >>= peekCString
        transport <- #{peek cci_device_t, transport} p >>= peekCString
        up <- #{peek cci_device_t, up} p
        info <- #{peek cci_device_t, info} p >>= \x -> if x == nullPtr then return "" else peekCString x
        conf_args <- (#{peek cci_device_t, conf_argv} p) >>= flip peekNullTerminated 0 >>= mapM peekCString
        max_send_size <- #{peek cci_device_t, max_send_size} p
        rate <- #{peek cci_device_t, rate} p
        pci_domain <- #{peek cci_device_t, pci.domain} p
        pci_bus <- #{peek cci_device_t, pci.bus} p
        pci_dev <- #{peek cci_device_t, pci.dev} p
        pci_func <- #{peek cci_device_t, pci.func} p
        return$ Device p DeviceInfo 
            { devName = name
            , devTransport = transport
            , devUp = up
            , devInfo = info
            , devConfArgs = conf_args
            , devMaxSendSize = max_send_size
            , devRate = rate
            , devPCI = (pci_domain, pci_bus, pci_dev, pci_func)
            }


foreign import ccall unsafe cci_get_devices :: Ptr (Ptr (Ptr Device)) -> IO CInt



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
-- before calling 'createBlockingEndpoint'. This is probably done by starting
-- the thread with 'Control.Concurrent.forkOn'.
--
-- The OS handle returned by 'createBlockingEndpoint' can be manipulated with
-- 'Control.Concurrent.threadWaitRead' and 'Control.Concurrent.threadWaitWrite'.
--
-- May throw:
--
--  * 'ENODEV' if the device is not \"up\"
--
--  * 'ENOMEM' if not enough memory is available
--
--  * driver-specific errors
--
createBlockingEndpoint :: Maybe Device -- ^ The device to use or Nothing to use the system-default device.
               -> IO (Endpoint,Fd) -- ^ The endpoint and an operating system handle that can be used
                                   -- to block for progress on this endpoint.
createBlockingEndpoint mdev = alloca$ \ppend ->
    alloca$ \pfd -> do
      cci_create_endpoint (maybe nullPtr devPtr mdev) 0 ppend pfd >>= cci_check_exception
      liftM2 ((,)) (peek ppend) (fmap Fd$ peek pfd)


-- | Like 'createBlockingEndpoint' but avoids creation of an OS handle. Use this function
-- only if you intend to poll for events without blocking.
createPollingEndpoint :: Maybe Device -- ^ The device to use or Nothing to use the system-default device
               -> IO Endpoint -- ^ The endpoint
createPollingEndpoint mdev = alloca$ \ppend -> do
      cci_create_endpoint (maybe nullPtr devPtr mdev) 0 ppend nullPtr >>= cci_check_exception
      peek ppend


foreign import ccall unsafe cci_create_endpoint :: Ptr Device -> CInt -> Ptr Endpoint -> Ptr CInt -> IO CInt


-- | Frees resources associated with the endpoint. All open connections 
-- are closed immediately. It is exactly as if 'disconnect' was 
-- invoked on every open connection on this endpoint.
--
-- May throw driver-specific errors.
destroyEndpoint :: Endpoint -> IO ()
destroyEndpoint (Endpoint pend) = cci_destroy_endpoint pend >>= cci_check_exception

foreign import ccall unsafe cci_destroy_endpoint :: Ptr EndpointV -> IO CInt


-- | Wraps an IO action with calls to 'createBlockingEndpoint' and 'destroyEndpoint'.
--
-- Makes sure that 'destroyEndpoint' is called in the presence of errors.
--
withBlockingEndpoint :: Maybe Device -> ((Endpoint,Fd) -> IO a) -> IO a
withBlockingEndpoint mdev = bracket (createBlockingEndpoint mdev) (destroyEndpoint . fst)

-- | Wraps an IO action with calls to 'createPollingEndpoint' and 'destroyEndpoint'.
--
-- Makes sure that 'destroyEndpoint' is called in the presence of errors.
--
withPollingEndpoint :: Maybe Device -> (Endpoint -> IO a) -> IO a
withPollingEndpoint mdev = bracket (createPollingEndpoint mdev) destroyEndpoint


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
connMaxSendSize :: Connection -> Word32
connMaxSendSize (Connection pconn) = unsafePerformIO$ #{peek cci_connection_t, max_send_size} pconn

-- | Local endpoint on which the connection was created
connEndpoint :: Connection -> Endpoint
connEndpoint (Connection pconn) = Endpoint$ unsafePerformIO$ #{peek cci_connection_t, endpoint} pconn

-- | Attributes of the connection
connAttributes :: Connection -> ConnectionAttributes
connAttributes (Connection pconn) = toEnum$ unsafePerformIO$ #{peek cci_connection_t, attribute} pconn

-- | Context provided when the connection was created
connContext :: Connection -> WordPtr
connContext (Connection pconn) = unsafePerformIO$ #{peek cci_connection_t, context} pconn


-- | Accepts a connection request and establish a connection with a 
-- specific endpoint.
--
-- May throw:
--
--  * 'EINVAL' if the Event was not a EvConnectRequest
--
--  * driver-specific errors
--
accept :: Event s  -- ^ A connection request event previously returned by 'getEvent'
       -> WordPtr  -- ^ A context value which is passed in an 'EvAccept' event.
       -> IO ()
accept (Event penv) pctx =
    cci_accept penv (wordPtrToPtr pctx) >>= cci_check_exception

foreign import ccall unsafe cci_accept :: Ptr EventV -> Ptr () -> IO CInt



-- | Rejects a connection request.
--
-- May throw:
--
--  * 'EINVAL' if the Event was not a 'EvConnectRequest'
--
--  * driver-specific errors
--
reject :: Event s -> IO ()
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
         in allocaBytesAligned #{size struct timeval} #{alignment struct timeval}$ \ptv -> do
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
-- A short message limited to the size of 'connMaxSendSize'.
--
-- If the application needs to send a message larger than
-- 'connMaxSendSize', the application is responsible for
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
-- May throw 'ERR_DISCONNECTED', 'ERR_RNR', or driver-specific errors.
--
send :: Connection -> ByteString -> WordPtr -> IO ()
send conn msg pctx = unsafeUseAsCStringLen msg$ \cmsg -> send' conn cmsg pctx 0 

-- | Like 'send' but will also block until the send completion has occurred.
--
-- In this case, there is no event returned for this send via 'getEvent', the
-- send completion status is delivered in a 'CCIException' if sending fails.
-- A safe foreign call is made when using this flag.
--
sendBlocking :: Connection -> ByteString -> WordPtr -> IO ()
sendBlocking (Connection pconn) msg pctx = unsafeUseAsCStringLen msg$ \(cmsg,clen) -> 
     safe_cci_send pconn cmsg (fromIntegral clen) (wordPtrToPtr pctx) #{const CCI_FLAG_BLOCKING}
           >>= cci_check_exception

-- | Like 'send' but no send completion will be generated. 
-- 
-- For reliable ordered connections, since completions are issued in order, the 
-- completion of any non-SILENT send directly implies the completion of any
-- previous SILENT sends. For unordered connections, completion ordering is
-- not guaranteed -- it is not safe to assume that application protocol
-- semantics imply specific unordered SILENT send completions. The
-- only ways to know when unordered SILENT sends have completed is
-- either to close the connection or issue a non-SILENT send. The
-- completion of a non-SILENT send guarantees the completion of all
-- previous SILENT sends.
--
sendSilent :: Connection -> ByteString -> WordPtr -> IO ()
sendSilent conn msg pctx = unsafeUseAsCStringLen msg$ \cmsg -> send' conn cmsg pctx #{const CCI_FLAG_SILENT}

-- | Like 'send' but the message buffer remains in use until the send completion occurs. 
--
-- The CCI implementation is therefore free to use \"zero copy\" types 
-- of transmission with the buffer if it wants to.
--
sendNoCopy :: Connection -> CStringLen -> WordPtr -> IO ()
sendNoCopy conn msg pctx = send' conn msg pctx #{const CCI_FLAG_NO_COPY}

-- | Like 'sendNoCopy' and 'sendSilent'.
sendNoCopySilent :: Connection -> CStringLen -> WordPtr -> IO ()
sendNoCopySilent conn msg pctx = send' conn msg pctx (#{const CCI_FLAG_NO_COPY} .|. #{const CCI_FLAG_SILENT})


send' :: Connection -> CStringLen -> WordPtr -> CInt -> IO ()
send' (Connection pconn) (cmsg,clen) pctx flags = 
     cci_send pconn cmsg (fromIntegral clen) (wordPtrToPtr pctx) flags
           >>= cci_check_exception

foreign import ccall unsafe cci_send :: Ptr ConnectionV -> Ptr CChar -> Word32 -> Ptr () -> CInt -> IO CInt
foreign import ccall safe "cci_send" safe_cci_send :: Ptr ConnectionV -> Ptr CChar -> Word32 -> Ptr () -> CInt -> IO CInt



-- | Send a short vectored (gather) message.
--
-- Like 'send', 'sendv' sends a short message bound by
-- 'connMaxSendSize'. Instead of a single data buffer,
-- 'sendv' allows the application to gather a list of 
-- ByteStrings.
--
-- May throw driver-specific errors.
--
sendv :: Connection -> [ByteString] -> WordPtr -> IO ()
sendv conn msgs pctx = unsafeUseAsCStringLens msgs [] 0$ \cmsgs clen -> 
    sendv' conn cmsgs clen pctx 0

-- | Like 'sendv' and 'sendBlocking'.
sendvBlocking :: Connection -> [ByteString] -> WordPtr -> IO ()
sendvBlocking (Connection pconn) msgs pctx = unsafeUseAsCStringLens msgs [] 0$ \cmsgs clen -> 
     allocaBytesAligned (clen * #{size struct iovec}) #{alignment struct iovec}$ \piovecs -> do
          sequence_ (zipWith (write_iovec piovecs) [clen-1,clen-2..0] cmsgs) 
          safe_cci_sendv pconn piovecs (fromIntegral clen) (wordPtrToPtr pctx) #{const CCI_FLAG_BLOCKING}
             >>= cci_check_exception

-- | Like 'sendv' and 'sendSilent'.
sendvSilent :: Connection -> [ByteString] -> WordPtr -> IO ()
sendvSilent conn msgs pctx = unsafeUseAsCStringLens msgs [] 0$ \cmsgs clen ->
    sendv' conn cmsgs clen pctx #{const CCI_FLAG_SILENT}

-- | Like 'sendv' and 'sendNoCopySilent'.
sendvNoCopy :: Connection -> [CStringLen] -> WordPtr -> IO ()
sendvNoCopy conn cmsgs pctx = sendv' conn cmsgs (Prelude.length cmsgs) pctx #{const CCI_FLAG_NO_COPY}

-- | Like 'sendv' and 'sendNoCopySilent'.
sendvNoCopySilent :: Connection -> [CStringLen] -> WordPtr -> IO ()
sendvNoCopySilent conn cmsgs pctx = 
    sendv' conn cmsgs (Prelude.length cmsgs) pctx (#{const CCI_FLAG_NO_COPY} .|. #{const CCI_FLAG_SILENT})

sendv' :: Connection -> [CStringLen] -> Int -> WordPtr -> CInt -> IO ()
sendv' (Connection pconn) cmsgs clen pctx flags = 
     allocaBytesAligned (clen * #{size struct iovec}) #{alignment struct iovec}$ \piovecs -> do
          sequence_ (zipWith (write_iovec piovecs) [clen-1,clen-2..0] cmsgs) 
          cci_sendv pconn piovecs (fromIntegral clen) (wordPtrToPtr pctx) flags
             >>= cci_check_exception

unsafeUseAsCStringLens :: [ByteString] -> [CStringLen] -> Int -> ([CStringLen] -> Int -> IO a) -> IO a
unsafeUseAsCStringLens (x:xs) acc len f
        | seq len True = unsafeUseAsCStringLen x$ \cx -> unsafeUseAsCStringLens xs (cx:acc) (len+1) f
unsafeUseAsCStringLens _ acc len f = f acc len

write_iovec :: Ptr a -> Int -> CStringLen -> IO ()
write_iovec piovecs offs (cmsg,clen) = do
    let p = plusPtr piovecs (offs * #{size struct iovec})
    #{poke struct iovec, iov_base} p cmsg
    #{poke struct iovec, iov_len} p clen


foreign import ccall unsafe cci_sendv :: Ptr ConnectionV -> Ptr () -> Word32 -> Ptr () -> CInt -> IO CInt
foreign import ccall safe "cci_sendv" safe_cci_sendv :: Ptr ConnectionV -> Ptr () -> Word32 -> Ptr () -> CInt -> IO CInt

enumFlags :: (Enum a, Num b) => [a] -> b
enumFlags = fromIntegral . foldl (\f-> (f.|.) . fromEnum) (0::Int)


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
-- May throw driver-specific errors.
--
rmaRegister :: Endpoint -> CStringLen -> RMA_MODE -> IO RMALocalHandle
rmaRegister (Endpoint pend) (cbuf,clen) m = alloca$ \p ->
    cci_rma_register pend cbuf (fromIntegral clen) (fromIntegral (fromEnum m)) p
      >>= cci_check_exception >> fmap RMALocalHandle (peek p)

foreign import ccall unsafe cci_rma_register :: Ptr EndpointV 
                                             -> Ptr CChar -> Word64 -> CInt -> Ptr (Ptr CChar) -> IO CInt

-- | Mode for registered handles.
data RMA_MODE =
      RMA_READ        -- ^ Handle can be read by other endpoints.
    | RMA_WRITE       -- ^ Handle can be written by other endpoints.
    | RMA_READ_WRITE  -- ^ Handle can be both read and written by other endpoints.


instance Enum RMA_MODE where

  fromEnum RMA_READ = #const CCI_FLAG_READ
  fromEnum RMA_WRITE = #const CCI_FLAG_WRITE
  fromEnum RMA_READ_WRITE = #{const CCI_FLAG_READ} .|. #{const CCI_FLAG_WRITE}

  toEnum #{const CCI_FLAG_READ} = RMA_READ
  toEnum #{const CCI_FLAG_WRITE} = RMA_WRITE
  toEnum i | i == #{const CCI_FLAG_READ} .|. #{const CCI_FLAG_WRITE} = RMA_READ_WRITE
  toEnum i = error$ "RMA_MODE_FLAG toEnum: unknown value: "++show i




-- | Deregisters memory.
--
-- If an RMA is in progress that uses this handle, the RMA may abort or
-- the deregisteration may fail.
--
-- May throw driver-specific errors.
--
rmaDeregister :: Endpoint -> RMALocalHandle -> IO ()
rmaDeregister (Endpoint ep) (RMALocalHandle p) = cci_rma_deregister ep p >>= cci_check_exception

foreign import ccall unsafe cci_rma_deregister :: Ptr EndpointV -> Ptr CChar -> IO CInt


-- | Wraps an IO operation with calls to 'rmaRegister' and 'rmaDeregister'.
--
-- This function makes sure to call 'rmaDeregister' even in the presence of errors.
--
withRMALocalHandle :: Endpoint -> CStringLen -> RMA_MODE -> (RMALocalHandle -> IO a) -> IO a
withRMALocalHandle e cs m = bracket (rmaRegister e cs m) (rmaDeregister e)


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
-- Optionally, sends a remote completion event to the target. If a 'ByteString'
-- message is provided, send a completion event to the target after
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
rmaRead (Connection pconn) mb (RMALocalHandle lhp) lo (RMARemoteHandle rh) ro dlen pctx flags = 
  unsafeUseAsCString rh$ \rhp ->
   let crma (cb,clen) = cci_rma pconn cb (fromIntegral clen) lhp lo rhp ro dlen 
                                (wordPtrToPtr pctx) (#{const CCI_FLAG_READ} .|. enumFlags flags) 
                            >>= cci_check_exception
    in case mb of
        Just b -> unsafeUseAsCStringLen b$ crma
        Nothing -> crma (nullPtr,0::Int)

foreign import ccall unsafe cci_rma :: Ptr ConnectionV -> Ptr CChar -> Word32 
                                    -> Ptr CChar -> Word64 -> Ptr CChar -> Word64 -> Word64 
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
rmaWrite (Connection pconn) mb (RMARemoteHandle rh) ro (RMALocalHandle lhp) lo dlen pctx flags = 
  unsafeUseAsCString rh$ \rhp ->
   let crma (cb,clen) = cci_rma pconn cb (fromIntegral clen) lhp lo rhp ro dlen 
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
newtype RMALocalHandle = RMALocalHandle (Ptr CChar)
  deriving Eq

-- | RMA remote handles have an associated buffer in a remote location
-- which is read or written during RMA operations.
--
newtype RMARemoteHandle = RMARemoteHandle ByteString


-- | Gets a ByteString representation of the handle which can be sent to a peer.
rmaHandle2ByteString :: RMALocalHandle -> IO ByteString
rmaHandle2ByteString (RMALocalHandle p) = packCStringLen (p,32)

-- | Creates a remote handle from a ByteString representation of a remote
-- handle. It is the inverse 'rmaHandle2ByteString'.
createRMARemoteHandle :: ByteString -> Maybe RMARemoteHandle
createRMARemoteHandle b = case B.length b of
                            32 -> Just$ RMARemoteHandle b
                            _  -> Nothing


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
getEvent :: Endpoint -> IO (Maybe (Event s))
getEvent (Endpoint pend) = alloca$ \pev -> do
    st <- cci_get_event pend pev
    case st of
      #{const CCI_EAGAIN} -> return Nothing
      _ -> cci_check_exception st >> fmap Just (peek pev)


foreign import ccall unsafe cci_get_event :: Ptr EndpointV -> Ptr (Event s) -> IO CInt


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
returnEvent :: Event s -> IO ()
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
tryWithEventData ::
   Endpoint   -- ^ Endpoint on which to listen for events.
   -> IO a    -- ^ Action to perform in case no event is available.
   -> (forall s. EventData s -> IO a) -- ^ Callback for the case when an event is available.
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
withEventData :: 
   Endpoint   -- ^ Endpoint on which to listen for events.
   -> Fd      -- ^ OS handle to block onto.
   -> (forall s. EventData s -> IO a) 
   -> IO a  -- ^ Yields the callback result.
withEventData endp fd f = go
  where
    go = tryWithEventData endp (threadWaitRead fd >> go) f


-- | Like 'tryWithEventData' but loops polling until events are available.
pollWithEventData :: 
   Endpoint   -- ^ Endpoint on which to listen for events.
   -> (forall s. EventData s -> IO a) 
   -> IO a  -- ^ Yields the callback result.
pollWithEventData endp f = go
  where
    go = tryWithEventData endp go f


-- | Event representation.
--
-- The @s@ parameter encodes the scope in which the event is valid. 
-- It prevents the Event from escaping the 'withEventData'-like functions
-- which clearly define its lifetime. The protection is limited though.
-- Launching threads from these functions or using existential types 
-- would sidestep the constraint.
--
newtype Event s = Event (Ptr EventV)
  deriving (Storable, Show)

data EventV


-- | Retrieves the public data contained in an event.
getEventData :: Event s -> IO (EventData s)
getEventData ev@(Event pev) = do
    t <- #{peek cci_event_t, type} pev
    case t::CInt of
      #{const CCI_EVENT_SEND} -> 
          liftM3 EvSend (#{peek cci_event_send_t, context} pev)
                        (fmap ctoEnum$ #{peek cci_event_send_t, status} pev)
                        (#{peek cci_event_send_t, connection} pev)
      #{const CCI_EVENT_RECV} -> 
          liftM2 EvRecv (join$ liftM2 mkEventBytes
                                      (#{peek cci_event_recv_t, ptr} pev) 
                                      (#{peek cci_event_recv_t, len} pev))
                        (#{peek cci_event_recv_t, connection} pev)
      #{const CCI_EVENT_CONNECT} -> do
          st <- #{peek cci_event_connect_t, status} pev
          liftM2 EvConnect (fmap ptrToWordPtr$ #{peek cci_event_connect_t, context} pev) 
                           (toEither st$ #{peek cci_event_connect_t, connection} pev)
      #{const CCI_EVENT_ACCEPT} -> do
          st <- #{peek cci_event_accept_t, status} pev
          liftM2 EvAccept (fmap ptrToWordPtr$ #{peek cci_event_accept_t, context} pev) 
                          (toEither st$ #{peek cci_event_accept_t, connection} pev)
      #{const CCI_EVENT_CONNECT_REQUEST} -> 
          liftM2 (EvConnectRequest ev) (join$ liftM2 mkEventBytes
                                              (#{peek cci_event_connect_request_t, data_ptr} pev) 
                                              (#{peek cci_event_connect_request_t, data_len} pev))
                                       (fmap ctoEnum$ #{peek cci_event_connect_request_t, attribute} pev)
      #{const CCI_EVENT_KEEPALIVE_TIMEDOUT} -> 
          fmap EvKeepAliveTimedOut (#{peek cci_event_keepalive_timedout_t, connection} pev) 
      #{const CCI_EVENT_ENDPOINT_DEVICE_FAILED} -> 
          fmap EvEndpointDeviceFailed (#{peek cci_event_endpoint_device_failed_t, endpoint} pev) 
          
      _ -> error$ "getEventData: unexpected event type: "++show t
  where
    mkEventBytes p len = return$ EventBytes (p,fromIntegral (len::CInt))
    ctoEnum :: Enum a => CInt -> a
    ctoEnum = toEnum . fromIntegral
    toEither :: CInt -> IO Connection -> IO (Either Status Connection)
    toEither #{const CCI_SUCCESS} c = fmap Right c
    toEither st                   _ = return$ Left$ ctoEnum st 



-- | Bytes owned by an 'Event'. They will be released when the event is returned.
--
-- The @s@ parameter encodes the scope in which the event is valid. 
-- It prevents the EventBytes from escaping the 'withEventData'-like functions
-- which clearly define their lifetime. The protection is limited though.
-- Launching threads from these functions or using existential types 
-- would sidestep the constraint.
--
newtype EventBytes s = EventBytes CStringLen
  deriving Show


-- | Copies the event bytes into a ByteString.
packEventBytes :: EventBytes s -> IO ByteString
packEventBytes (EventBytes bs) = packCStringLen bs


-- | Wraps the event bytes into a ByteString. The ByteString is valid for as long
-- as the given event bytes are valid.
unsafePackEventBytes :: EventBytes s -> IO ByteString
unsafePackEventBytes (EventBytes bs) = unsafePackCStringLen bs


-- | Representation of data contained in events.
--
-- The @s@ parameter encodes the scope in which the event is valid. 
-- It prevents the EventData from escaping the 'withEventData'-like functions
-- which clearly define their lifetime. The protection is limited though.
-- Launching threads from these functions or using existential types 
-- would sidestep the constraint.
--
data EventData s =

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
  | EvRecv (EventBytes s) Connection

    -- | A new outgoing connection was successfully accepted at the
    -- peer; a connection is now available for data transfer.
    -- If a connection request is rejected or fails otherwise,
    -- an error code will be provided instead of a connection.
    --
    -- Contains the context given to 'connect'.
    --
  | EvConnect WordPtr (Either Status Connection)

    -- | A new incoming connection was successfully accepted
    -- provided that the status field is 'SUCCESS';
    -- a connection is now available for data transfer.
    -- If accepting the connection fails for whatever reason,
    -- an error code will be provided instead of a connection.
    --
    -- Contains the context given to 'accept'.
    --
  | EvAccept WordPtr (Either Status Connection)

    -- | An incoming connection request from a client.
    --
    -- Contains the data transmitted with the request, the
    -- connection attributes and a reference to the event
    -- for convenience to call 'accept' and 'reject'.
    --
  | EvConnectRequest (Event s) (EventBytes s) ConnectionAttributes

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
getEndpt_KeepAliveTimeout :: Endpoint -> IO Word32
getEndpt_KeepAliveTimeout = getEndpointOpt' #{const CCI_OPT_ENDPT_KEEPALIVE_TIMEOUT} peek

-- | See 'getEndpt_KeepAliveTimeout'.
setEndpt_KeepAliveTimeout :: Endpoint -> Word32 -> IO ()
setEndpt_KeepAliveTimeout = setEndpointOpt' #{const CCI_OPT_ENDPT_KEEPALIVE_TIMEOUT} poke


-- | Default send timeout for all new connections.
getEndpt_SendTimeout :: Endpoint -> IO Word32
getEndpt_SendTimeout = getEndpointOpt' #{const CCI_OPT_ENDPT_SEND_TIMEOUT} peek

-- | See 'getEndpt_SendTimeout'.
setEndpt_SendTimeout :: Endpoint -> Word32 -> IO ()
setEndpt_SendTimeout = setEndpointOpt' #{const CCI_OPT_ENDPT_SEND_TIMEOUT} poke

-- | How many send buffers on the endpoint. It is the max number of
-- pending messages the CCI layer can buffer before failing or
-- blocking (depending on reliability mode).
getEndpt_SendBufCount :: Endpoint -> IO Word32
getEndpt_SendBufCount = getEndpointOpt' #{const CCI_OPT_ENDPT_SEND_BUF_COUNT} peek

-- | See 'getEndpt_SendBufCount'.
setEndpt_SendBufCount :: Endpoint -> Word32 -> IO ()
setEndpt_SendBufCount = setEndpointOpt' #{const CCI_OPT_ENDPT_SEND_BUF_COUNT} poke

-- | How many receiver buffers on the endpoint. It is the max
-- number of messages the CCI layer can receive without dropping.
getEndpt_RecvBufCount :: Endpoint -> IO Word32
getEndpt_RecvBufCount = getEndpointOpt' #{const CCI_OPT_ENDPT_RECV_BUF_COUNT} peek

-- | See 'getEndpt_RecvBufCount'.
setEndpt_RecvBufCount :: Endpoint -> Word32 -> IO ()
setEndpt_RecvBufCount = setEndpointOpt' #{const CCI_OPT_ENDPT_RECV_BUF_COUNT} poke

-- | RMA registration alignment requirements can be retrieved for an endpoint.
--
-- The CTP will yield the minimal alignment needed for different operations.
-- A value of 0 indicates that there are no alignment requirements for that 
-- operation. A value of 4, for example, indicates that that member must be 4-byte
-- aligned.
--
-- If the CTP requires RMA alignment and the application passes in an
-- un-aligned parameter, the CTP may need to allocate a temporary
-- buffer, register it, and use it instead. This will also require a
-- copy of the data to the correct location. This will decrease
-- performance for these cases.
--
getEndpt_RMAAlign :: Endpoint -> IO RMAAlignments
getEndpt_RMAAlign = getEndpointOpt' #{const CCI_OPT_ENDPT_RMA_ALIGN} peek

-- | Retrieve the endpoint's URI used for listening for connection
-- requests. The application should never need to parse this URI.
getEndpt_URI :: Endpoint -> IO String
getEndpt_URI = getEndpointOpt' #{const CCI_OPT_ENDPT_URI} ((peekCString =<<) . peek)


setEndpointOpt' :: Storable a => CInt -> (Ptr a -> a -> IO ()) -> Endpoint ->  a -> IO ()
setEndpointOpt' opt pk (Endpoint pend) v = withValueO pk v$ \pv ->
      cci_set_opt (castPtr pend) opt (castPtr pv) >>= cci_check_exception

getEndpointOpt' :: Storable a => CInt -> (Ptr a -> IO b) -> Endpoint -> IO b
getEndpointOpt' opt pk (Endpoint pend) =
    alloca$ \pv -> do
      cci_get_opt (castPtr pend) opt (castPtr pv) >>= cci_check_exception
      pk pv


foreign import ccall unsafe cci_set_opt :: Ptr () -> CInt -> Ptr () -> IO CInt
foreign import ccall unsafe cci_get_opt :: Ptr () -> CInt -> Ptr () -> IO CInt


data RMAAlignments = RMAAlignments 
    { rmaWriteLocalAddr :: Word32
    , rmaWriteRemoteAddr :: Word32
    , rmaWriteLength :: Word32
    , rmaReadLocalAddr :: Word32
    , rmaReadRemoteAddr :: Word32
    , rmaReadLength :: Word32
    }
  deriving Show

instance Storable RMAAlignments where
  sizeOf _ = #{size cci_alignment_t}
  alignment _ = #{alignment cci_alignment_t}
  peek p = do
      rwla <- #{peek cci_alignment_t, rma_write_local_addr} p
      rwra <- #{peek cci_alignment_t, rma_write_remote_addr} p
      rwl  <- #{peek cci_alignment_t, rma_write_length} p
      rrla <- #{peek cci_alignment_t, rma_read_local_addr} p
      rrra <- #{peek cci_alignment_t, rma_read_remote_addr} p
      rrl  <- #{peek cci_alignment_t, rma_read_length} p
      return RMAAlignments
        { rmaWriteLocalAddr  = rwla
        , rmaWriteRemoteAddr = rwra
        , rmaWriteLength     = rwl
        , rmaReadLocalAddr   = rrla
        , rmaReadRemoteAddr  = rrra
        , rmaReadLength      = rrl
        }
  poke = error "poke RMAAlignments: unimplemented"

withValueO :: Storable a => (Ptr a -> a -> IO ()) -> a -> (Ptr a -> IO b) -> IO b
withValueO pk v f = alloca$ \pv -> pk pv v >> f pv


--------------------------
-- Connection options
--------------------------

-- | Reliable send timeout in microseconds.
getConn_SendTimeout :: Connection -> IO Word32
getConn_SendTimeout = getConnectionOpt' #{const CCI_OPT_CONN_SEND_TIMEOUT} peek

-- | See 'getConn_SendTimeout'.
setConn_SendTimeout :: Connection -> Word32 -> IO ()
setConn_SendTimeout = setConnectionOpt' #{const CCI_OPT_CONN_SEND_TIMEOUT} poke


setConnectionOpt' :: Storable a => CInt -> (Ptr a -> a -> IO ()) -> Connection -> a -> IO ()
setConnectionOpt' opt pk (Connection pconn) v = withValueO pk v$ \pv ->
      cci_set_opt (castPtr pconn) opt (castPtr pv) >>= cci_check_exception


getConnectionOpt' :: Storable a => CInt -> (Ptr a -> IO b) -> Connection -> IO b
getConnectionOpt' opt pk (Connection pconn) =
    alloca$ \pv -> do
      cci_get_opt (castPtr pconn) opt (castPtr pv) >>= cci_check_exception
      pk pv



------------------------------------------
--
-- Error handling
--
------------------------------------------


-- | Returns a human readable description of a 'Status' value.
strError :: Maybe Endpoint -> Status -> IO String
strError me st = do
     cs <- cci_strerror (maybe nullPtr (\(Endpoint ep) -> ep) me)$ fromIntegral$ fromEnum st
     if nullPtr == cs then return ""
       else peekCString cs

foreign import ccall unsafe cci_strerror :: Ptr EndpointV -> CInt -> IO CString


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
   -- 
   -- One other place where @ETIMEDOUT@ appears is when an outgoing
   -- connection request expires (in an 'EvConnect' event).
  | ETIMEDOUT
  | ECONNREFUSED
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
     ECONNREFUSED        -> #{const CCI_ECONNREFUSED}
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
     #{const CCI_ECONNREFUSED}        -> ECONNREFUSED
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
    


