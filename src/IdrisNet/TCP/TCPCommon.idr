module IdrisNet.TCP.TCPCommon
import Network.Socket

public export
data SocketOperationRes a = OperationSuccess a
                          | FatalError SocketError -- Most socket errors are fatal.
                          | RecoverableError SocketError -- EAGAIN / EWOULDBLOCK
                          | ConnectionClosed


