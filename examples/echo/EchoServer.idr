module Main
import Effects
import Effect.StdIO
import Effect.State
import Effect.System
import Network.Socket
import IdrisNet.TCP.TCPServer
import IdrisNet.TCP.TCPCommon


receive : { [TCPSERVERCLIENT ClientConnected, STDIO, SYSTEM] ==>
            [TCPSERVERCLIENT (), STDIO, SYSTEM] } Eff ()
receive = do
  t <- time
  putStr (show t ++ ": Waiting for a message\n")
  -- Receive
  OperationSuccess (str, len) <- tcpRecv 1024
    | RecoverableError _ => receive
    | FatalError err => do putStr ("Error receiving: " ++ (show err))
                           tcpFinalise
    | ConnectionClosed => pure ()
  -- Echo
  OperationSuccess _ <- tcpSend $ str
    | RecoverableError err => do putStr ("Error sending: " ++ (show err))
                                 tcpClose
    | FatalError err => do putStr ("Error sending: " ++ (show err))
                           tcpFinalise
    | ConnectionClosed => pure ()
  receive


forkServerLoop : { [STDIO, TCPSERVER (ServerListening)] ==>
               [STDIO, TCPSERVER ()] } Eff ()
forkServerLoop = do
  -- Accept, and perform the "receive" program with the new socket.
  OperationSuccess _ <- forkAccept receive [default, default]
       | RecoverableError _ => forkServerLoop
       | FatalError err => do putStr ("Error accepting: " ++ (show err))
                              finaliseServer
       | ConnectionClosed => pure ()
  forkServerLoop

serverLoop : { [STDIO, TCPSERVER (ServerListening)] ==>
               [STDIO, TCPSERVER ()] } Eff ()
serverLoop = do
  -- Accept, and perform the "receive" program with the new socket.
  OperationSuccess _ <- accept receive [default, default]
    | RecoverableError _ => serverLoop
    | FatalError err => do putStr ("Error accepting: " ++ (show err))
                           finaliseServer
    | ConnectionClosed => pure ()
  serverLoop

setupServer : Port -> Bool ->
              { [STDIO, TCPSERVER ()] } Eff ()
setupServer port do_fork = do
  putStr "Binding\n"
  OperationSuccess _ <- bind Nothing port
    | RecoverableError _ => pure ()
    | FatalError err => do putStr ("Error binding: " ++ (show err) ++ "\n")
                           pure ()
    | ConnectionClosed => pure ()
  putStr "Bound\n"
  OperationSuccess _ <- listen
    | RecoverableError err => do putStr ("Recoverable error: " ++ (show err))
                                 closeBound
    | FatalError err => do putStr ("Error binding: " ++ show err)
                           finaliseServer
    | ConnectionClosed => pure ()
  putStr "Listening\n"
  if do_fork then forkServerLoop else serverLoop


main : IO ()
main = run (setupServer 1234 True)
