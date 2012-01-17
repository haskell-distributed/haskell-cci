
import Control.Exception     ( finally )
import Data.ByteString as B  ( putStrLn, empty )
import Data.ByteString.Char8 ( pack )
import System.Environment    ( getArgs )

import Network.CCI           ( initCCI, withEndpoint, connect, ConnectionAttributes(..)
                             , pollWithEventData, EventData(..), disconnect, send
                             , unsafePackEventBytes
                             )


main :: IO ()
main = do
    [uri] <- getArgs
    initCCI
    withEndpoint Nothing$ \(ep,fd) -> do
      connect ep uri empty CONN_ATTR_UU 0 Nothing
      print fd
      _ <- loopWhileM id$ pollWithEventData ep$ \ev -> 
         case ev of

           EvConnectAccepted ctx conn ->
             do print ctx
                send conn (pack "ping!") 1 []
                return True

           EvRecv ebs conn -> flip finally (disconnect conn)$
             do unsafePackEventBytes ebs >>= B.putStrLn
                return False

           _ -> print ev >> return True
      
      return ()


-- | @loopWhileM p io@ performs @io@ repeteadly while its result satisfies @p@.
-- Yields the first offending result.
loopWhileM :: (a -> Bool) -> IO a -> IO a
loopWhileM p io = io >>= \a -> if p a then loopWhileM p io else return a

