
import Control.Concurrent    ( threadWaitRead )
import Data.ByteString as B  ( ByteString, putStrLn, empty )
import Data.ByteString.Char8 ( pack )
import Network.CCI           ( initCCI, withEndpoint, getEvent, connect, ConnectionAttributes(..)
                             , withEventData, EventData(..), disconnect, send
                             )


main = do
    initCCI
    withEndpoint Nothing$ \(ep,fd) -> do
      connect ep "localhost" empty CONN_ATTR_UU "conn. req. ctx" Nothing
      loopWhileM id$ withEventData ep$ maybe (threadWaitRead fd >> return False)$ \ev -> 
         case ev of

           EvConnectAccepted "conn. req. cxt" conn ->
             do send conn (pack "ping!") "send client ctx." []
                return True

           EvRecv bs conn ->
             do B.putStrLn bs
                disconnect conn
                return True

           _ -> print ev >> return False
      
      return ()


-- | @loopWhileM p io@ performs @io@ repeteadly while its result satisfies @p@.
-- Yields the first offending result.
loopWhileM :: (a -> Bool) -> IO a -> IO a
loopWhileM p io = io >>= \a -> if p a then return a else loopWhileM p io

