--
-- Copyright (C) 2012 Parallel Scientific. All rights reserved.
--
-- See the accompanying COPYING file for license information.
--

import Control.Exception     ( finally )
import Control.Monad         ( forever, void )
import qualified Data.ByteString.Char8 as B ( putStrLn, pack )
import Network.CCI           ( withCCI, withPollingEndpoint, getEndpt_URI, accept
                             , pollWithEventData, EventData(..), disconnect, send
							 , unsafePackEventBytes
                             )


main :: IO ()
main =
  withCCI$
    withPollingEndpoint Nothing$ \ep -> do
      getEndpt_URI ep >>= putStrLn
      _ <- forever$ pollWithEventData ep$ \evd ->
         case evd of

           EvConnectRequest ev _bs _cattr -> void$ accept ev 0

           EvRecv ebs conn -> flip finally (disconnect conn)$
             do unsafePackEventBytes ebs >>= B.putStrLn
                send conn (B.pack "pong!") 1 []

           _ -> print evd
      
      return ()


