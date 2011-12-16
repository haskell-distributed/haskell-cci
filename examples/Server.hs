
import Control.Concurrent    ( threadWaitRead )
import Control.Monad         ( forever, void )
import Data.ByteString as B  ( ByteString, putStrLn )
import Data.ByteString.Char8 ( pack )
import Network.CCI           ( initCCI, withEndpoint, accept, ConnectionAttributes(..)
                             , withEventData, EventData(..), disconnect, send
                             )


main = do
    initCCI
    withEndpoint Nothing$ \(ep,fd) -> do
      forever$ withEventData ep$ maybe (threadWaitRead fd)$ \ev ->
         case ev of

           EvConnectRequest ev bs cattr -> void$ accept ev

           EvRecv bs conn ->
             do B.putStrLn bs
                send conn (pack "pong!") "send server ctx." []
                disconnect conn

           EvSend ctx status conn -> print ev

           _ -> print ev
      
      return ()


