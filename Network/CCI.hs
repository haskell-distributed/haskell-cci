{-# LANGUAGE DeriveDataTypeable, ExistentialQuantification, EmptyDataDecls #-}
module Network.CCI where

import Data.ByteString        ( ByteString, packCStringLen )
import Data.ByteString.Unsafe ( unsafePackCStringLen )
import Data.Dynamic           ( Typeable )
import Foreign.C              ( CStringLen )
import Data.Word              ( Word32 )
import System.Posix.Types     ( Fd )
import Control.Exception      ( Exception )

type URI = String

-- | Device representation
data Device

-- | Handle used to free devices
data DevicesHandle

-- | Endpoint representation
data Endpoint

-- | Connection representation
data Connection

-- | Maximum size of the messages the connection can send.
connectionMaxSendSize :: Connection -> Word32
connectionMaxSendSize = undefined


-- | Initializes CCI. If called more than once, only the
--   first call has any effects.
-- 
-- May throw:
--
--  * 'SUCCESS' CCI is available for use.
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
init :: IO ()
init = undefined



------------------------------------------

-- * Devices

------------------------------------------


-- | Returns a list of \"up\" devices.
--
-- The order of devices returned corresponds to the priority fields in
-- the devices. If two devices share the same priority, their
-- ordering in the return array is arbitrary.
--
-- Use the returned handle to freed resources associated with
-- the devices using 'freeDevices'.
--
-- Don't lose the DevicesHandle, or the garbage collector will call
-- 'freeDevices' before you are done with the returned devices.
--
-- Use 'withDevices' instead when it fits your purposes.
getDevices :: IO ([Device],DevicesHandle)
getDevices = undefined 

-- | Resources used by devices obtained with 'getDevices' are freed 
-- with this call.
--
-- Calling this on a DevicesHandles which has been freed already
-- has no effect.
--
-- Drivers may throw some error when freeing devices.
--
freeDevices :: DevicesHandle -> IO ()
freeDevices = undefined


-- | Calls 'getDevices' and 'freeDevices' around a given
-- block of code. in the presence of errors, 'freeDevices'
-- is guaranteed to be called.
withDevices :: ([Device] -> IO a) -> IO a
withDevices = undefined


-- | This function creates a CCI endpoint. A CCI endpoint represents a
-- collection of local resources (such as buffers and a completion
-- queue). An endpoint is associated with a device that performs the
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
createEndpoint = undefined


-- | Frees resources associated with the endpoint. All open connections 
-- are closed immediately. It is exactly as if 'disconnect' was 
-- invoked on every open connection on this endpoint.
--
-- May throw driver-specific errors.
destroyEndpoint :: Endpoint -> IO ()
destroyEndpoint = undefined



------------------------------------------

-- * Setting options

------------------------------------------


-- | Endpoint option representation
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


-- | Connection option representation
data ConnectionOption =
    -- | Reliable send timeout in microseconds.
    OPT_CONN_SEND_TIMEOUT 



-- | Sets a connection option value.
--
-- May throw:
--
--  * 'ERR_NOT_IMPLEMENTED' Not supported by this driver.
--
--  * driver-specific errors.
--
setConnectionOpt :: Connection -> ConnectionOption -> Word32 -> IO ()
setConnectionOpt = undefined

-- | Sets an endpoint option value
--
-- May throw:
--
--  * 'ERR_NOT_IMPLEMENTED' Not supported by this driver.
--
--  * driver-specific errors.
--
setEndpointOpt :: Endpoint -> EndpointOption -> Word32 -> IO ()
setEndpointOpt = undefined

-- | Retrieves a connection option value.
--
-- May throw:
--
--  * 'ERR_NOT_IMPLEMENTED' Not supported by this driver.
--
--  * driver-specific errors.
--
getConnectionOption :: Connection -> IO Word32
getConnectionOption = undefined

-- | Retrieves an endpoint option value.
--
-- May throw:
--
--  * 'ERR_NOT_IMPLEMENTED' Not supported by this driver.
-- 
--  * driver-specific errors.
--
getEndpointOption :: Endpoint -> EndpointOption -> IO Word32
getEndpointOption = undefined


------------------------------------------

-- * Connections

------------------------------------------


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
accept = undefined


-- | Rejects a connection request.
--
-- May throw:
--
--  * 'EINVAL' if the Event was not a 'EvConnectRequest'
--
--  * driver-specific errors
--
reject :: Event -> IO ()
reject = undefined


-- | Connection characteristics.
data ConnectionAttribute =
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



-- | Initiate a connection request (client side).
--
-- Request a connection from a specific endpoint. The server endpoint's address
-- is described by a Uniform Resource Identifier. The use of an URI allows for
-- flexible description (IP address, hostname, etc).
--
-- The connection request can carry limited amount of data to be passed to the
-- server for application-specific usage (identification, authentication, etc).
--
-- The connect call is always non-blocking, reliable and requires a decision
-- by the server (accept or reject), even for an unreliable connection, except
-- for multicast.
--
-- Multicast connections don't necessarily involve a discrete connection
-- server, they may be handled by IGMP or other distributed framework.
--
-- endpoint	Local endpoint to use for requested connection.
--
-- May throw device-specific errors.
--
connect :: Typeable ctx =>
           Endpoint  -- ^ Local endpoint to use for requested connection.
        -> URI       -- ^ Uniform Resource Identifier of the server and is
                     --   generated by the server's endpoint when it is created.
        -> ByteString  -- ^ Connection data to be send in the connection request
                       --   (for authentication, etc).
        -> ConnectionAttribute  -- ^ Attributes of the requested connection (reliability,
                                --   ordering, multicast, etc).
        -> ctx            -- ^ Context used to identify the connection later.
        -> Maybe Integer  -- ^ Nothing means \"forever\".
        -> IO ()
connect = undefined


-- | Tears down an existing connection.
--
-- Operation is local, remote side is not notified. From that point,
-- both local and remote side will get a 'ERR_DISCONNECTED' communication error
-- if sends are initiated on this connection.
--
-- May throw driver-specific errors.
disconnect :: Connection -> IO ()
disconnect = undefined



------------------------------------------

-- * Event handling

------------------------------------------

-- | Event representation
data Event


-- | Retrieves the public data contained in an event.
getEventData :: BufferHandler buffer => Event -> IO (EventData buffer)
getEventData = undefined

-- | Determines how a buffer is to be treated.
class BufferHandler buffer where
  -- | Creates a buffer from a 'CStringLen'
  mkBuffer :: CStringLen -> IO buffer

instance BufferHandler ByteString where
  mkBuffer = packCStringLen

-- | A ByteString which is valid as long as the
-- underlying buffer is not freed.
--
-- The underlying buffer is not managed by the garbage collector.
newtype VolatilByteString = VolatilB ByteString

instance BufferHandler VolatilByteString where
  mkBuffer = fmap VolatilB . unsafePackCStringLen


-- | Representation of data contained in events.
data EventData buffer =
    -- | A send or RMA has completed.
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
    forall ctx. Typeable ctx => EvSend ctx Status Connection    

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
  | forall ctx. Typeable ctx => EvConnectAccepted ctx Connection

    -- | A new outgoing connection did not complete the accept/connect
    -- handshake with the peer in the time specified as argument to
    -- 'connect'. CCI has therefore given up attempting to continue to 
    -- create this connection.
    --
    -- Contains the context given to 'connect'.
    --
  | forall ctx. Typeable ctx => EvConnectTimedOut ctx

    -- | A new outgoing connection was rejected by the server.
    --
    -- Contains the context given to 'connect'.
    --
  | forall ctx. Typeable ctx => EvConnectRejected ctx

    -- | An incoming connection request from a client.
    --
    -- Contains the data transmitted with the request and the
    -- connection attribute.
    --
  | EvConnectRequest buffer ConnectionAttribute

    -- | The keepalive timeout has expired, i.e. no data from the
    -- peer has been received during that time period
    -- (see'OPT_ENDPT_KEEPALIVE_TIMEOUT' for more details).
    --
  | EvKeepAliveTimedOut Connection

    -- | A device on this endpoint has failed.
    --
    -- Contains the endpoint on the device that failed.
  | EvEndpointDeviceFailed Endpoint



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
-- be explicitly returned later via cci_return_event().
--
-- May throw driver-specific errors.
--
-- The garbage collector will call 'returnEvent' if there are no
-- references to the returned event and memory is claimed.
--
getEvent :: Endpoint         -- ^ Endpoint to poll for a new event.
          -> IO (Maybe Event) -- ^ A new event if any.
getEvent = undefined


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
returnEvent = undefined


-- | Makes sure the event is returned to the CCI implementation
-- even in the presence of errors.
withEvent :: (Maybe Event -> IO a) -> IO a
withEvent = undefined


------------------------------------------

-- * Send and RMA

------------------------------------------

data SEND_FLAG =
    FLAG_BLOCKING 
  | FLAG_NO_COPY  
  | FLAG_SILENT 
  | FLAG_READ
  | FLAG_WRITE
  | FLAG_FENCE


-- | Send a short message.
--
-- A short message limited to the size of 'connectionMaxSendSize'.
--
-- If the application needs to send a message larger than
-- 'connectionMaxSendSize', the application is responsible for
-- segmenting and reassembly or it should use 'rma'.
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
--  * If the 'FLAG_BLOCKING' flag is specified, 'send' will also
--    block until the send completion has occurred. In this case, there
--    is no event returned for this send via 'getEvent', the send
--    completion status is returned via 'send'. A safe foreign call
--    is made when using this flag, if you intend to call this function
--    as blocking a lot, perhaps you should consider implementing the blocking 
--    behavior on the Haskell side.
--
--  * If the 'FLAG_NO_COPY' is specified, the application is
--    indicating that it does not need the buffer back until the send
--    completion occurs (which is most useful when FLAG_BLOCKING is
--    not specified). The CCI implementation is therefore free to use
--    \"zero copy\" types of transmission with the buffer -- if it wants to.
--
--  * 'FLAG_SILENT' means that no completion will be generated for
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
send :: Connection -> ByteString -> ctx -> [SEND_FLAG] -> IO ()
send = undefined




------------------------------------------

-- * Error handling

------------------------------------------


-- | Returns a human readable description of a 'Status' value.
strError :: Status -> IO String
strError = undefined


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
    -- that cci_disconnect() has been invoked on the send side (in
    -- which case this is an application error), or the receiver
    -- replied that the receiver invoked cci_disconnect().
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
 deriving (Show,Typeable)

