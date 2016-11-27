module Cshask where

import Network.Socket hiding (send)
import Network.Socket.ByteString (send)
import qualified Data.ByteString.Char8 as BS

write :: HostName -> ServiceName -> BS.ByteString -> IO ()
write ip port message = withSocketsDo $ do
  (server:_) <- getAddrInfo Nothing (Just ip) (Just port)
  sock <- socket (addrFamily server) Datagram defaultProtocol
  connect sock (addrAddress server)
  _ <- send sock message
  close sock

sendCsd :: String -> IO()
sendCsd message = write "localhost" "8000" (BS.pack message)

test :: String
test = "instr 1\na1 oscili p4*0dbfs,p5\nout a1\nendin\nschedule 1,0,1,0.5,440"

-- csound -odac --port=8000
