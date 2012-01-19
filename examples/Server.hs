--
-- Copyright (C) 2012 Parallel Scientific. All rights reserved.
--
-- See the accompanying COPYING file for license information.
--

import Control.Exception     ( finally )
import Control.Monad         ( forever, void )
import qualified Data.ByteString as B  ( putStrLn )
import Data.ByteString.Char8 ( pack )
import Network.CCI           ( initCCI, withEndpoint, endpointURI, accept
                             , pollWithEventData, EventData(..), disconnect, send
							 , unsafePackEventBytes
                             )


main :: IO ()
main = do
    initCCI
    withEndpoint Nothing$ \(ep,_fd) -> do
      endpointURI ep >>= putStrLn
      _ <- forever$ pollWithEventData ep$ \evd ->
         case evd of

           EvConnectRequest ev _bs _cattr -> void$ accept ev

           EvRecv ebs conn -> flip finally (disconnect conn)$
             do unsafePackEventBytes ebs >>= B.putStrLn
                send conn (pack "pong!") 1 []

           _ -> print evd
      
      return ()


