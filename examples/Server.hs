
import Control.Concurrent    ( threadWaitRead )
import Control.Exception     ( finally )
import Control.Monad         ( forever, void )
import qualified Data.ByteString as B  ( putStrLn )
import Data.ByteString.Char8 ( pack )
import Network.CCI           ( initCCI, withEndpoint, endpointURI, accept
                             , withEventData, EventData(..), disconnect, send
                             )


main :: IO ()
main = do
    initCCI
    withEndpoint Nothing$ \(ep,fd) -> do
      endpointURI ep >>= putStrLn
      _ <- forever$ withEventData ep$ maybe ({- threadWaitRead fd -} return ())$ \evd ->
         case evd of

           EvConnectRequest ev _bs _cattr -> void$ accept ev

           EvRecv bs conn -> flip finally (disconnect conn)$
             do B.putStrLn bs
                send conn (pack "pong!") 1 []

           _ -> print evd
      
      return ()


