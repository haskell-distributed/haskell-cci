--
-- Copyright (C) 2012 Parallel Scientific 
--
-- This file is part of cci-haskell.
--
-- cci-haskell is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License Version 2 as 
-- published by the Free Software Foundation.
--
-- cci-haskell is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with cci-haskell.  If not, see <http://www.gnu.org/licenses/>.
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


