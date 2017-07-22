#include <sys/socket.h>

module Graphics.Wayland.Internal.Socket
    ( Socket, fdSocket
    , sockStream, sockNonblock, sockCloexec
    , socket, bind, connect, listen, accept, close
    , recv, send
    , read, write
    , recvFd, sendFd
    ) where

import Prelude hiding (read)

import Control.Exception
import Control.Monad
import Data.Bits
import Data.Int
import Data.Word
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Ptr
import System.Posix.Types

newtype Socket = Socket Fd
    deriving (Eq, Ord, Show)

fdSocket :: Socket -> Fd
fdSocket (Socket fd) = fd

foreign import ccall "sys/socket.h socket"
    c_socket :: CInt -> CInt -> CInt -> IO CInt

sockStream :: CInt
sockStream = #{const SOCK_STREAM}
sockNonblock :: CInt
sockNonblock = #{const SOCK_NONBLOCK}
sockCloexec :: CInt
sockCloexec = #{const SOCK_CLOEXEC}

socket :: CInt -> IO Socket
socket sockType = fmap (Socket . Fd) $
    throwErrnoIfMinus1 "cannot create socket" $
        c_socket #{const AF_UNIX} (sockType .|. sockCloexec) 0

foreign import ccall "string.h strlen"
    c_strlen :: CString -> IO CInt

foreign import ccall "bind_unix"
    c_bind_unix :: CInt -> CString -> CInt -> IO CInt

bind :: Socket -> String -> IO ()
bind (Socket (Fd fd)) path = do
    str <- newCString path
    len <- c_strlen str
    when (len >= 108) $ ioError $ userError $ "bind: path too long: "++show path
    finally (throwErrnoIfMinus1_ "cannot bind socket" $ c_bind_unix fd str len)
            (free str)
    return ()

foreign import ccall "connect_unix"
    c_connect_unix :: CInt -> CString -> CInt -> IO CInt

connect :: Socket -> String -> IO ()
connect (Socket (Fd fd)) path = do
    str <- newCString path
    len <- c_strlen str
    when (len >= 108) $ ioError $ userError $ "bind: path too long: "++show path
    finally (throwErrnoIfMinus1_ "cannot bind socket" $ c_connect_unix fd str len)
            (free str)
    return ()

foreign import ccall "sys/socket.h listen"
    c_listen :: CInt -> CInt -> IO CInt

listen :: Socket -> CInt -> IO ()
listen (Socket (Fd fd)) backlog =
    throwErrnoIfMinus1_ "cannot listen socket" $ c_listen fd backlog

foreign import ccall "sys/socket.h accept4"
    c_accept4 :: CInt -> Ptr () -> Ptr () -> CInt -> IO CInt

accept :: Socket -> CInt -> IO Socket
accept (Socket (Fd fd)) flags = do
    newFd <- throwErrnoIfMinus1 "cannot accept socket" $
        c_accept4 fd nullPtr nullPtr flags
    return $ Socket $ Fd newFd

foreign import ccall "unistd.h close"
    c_close :: CInt -> IO CInt

close :: Socket -> IO ()
close (Socket (Fd fd)) = throwErrnoIfMinus1_ "cannot close socket" $ c_close fd

foreign import ccall "sys/socket.h recv"
    c_recv :: CInt -> Ptr Word8 -> #{type size_t} -> CInt -> IO #{type ssize_t}

recv :: Socket -> CInt -> Ptr Word8 -> Int -> IO Int
recv (Socket (Fd fd)) flags buf len =
    fromIntegral <$> c_recv fd buf (fromIntegral len) flags

read :: Socket -> Ptr Word8 -> Int -> IO Int
read sock = recv sock 0

foreign import ccall "sys/socket.h send"
    c_send :: CInt -> Ptr Word8 -> #{type size_t} -> CInt -> IO #{type ssize_t}

send :: Socket -> CInt -> Ptr Word8 -> Int -> IO Int
send (Socket (Fd fd)) flags buf len =
    fromIntegral <$> c_send fd buf (fromIntegral len) flags

write :: Socket -> Ptr Word8 -> Int -> IO Int
write sock = send sock 0

foreign import ccall "recv_fd"
    c_recv_fd :: CInt -> IO CInt

recvFd :: Socket -> IO CInt
recvFd (Socket (Fd fd)) = c_recv_fd fd

foreign import ccall "send_fd"
    c_send_fd :: CInt -> CInt -> IO CInt

sendFd :: Socket -> Fd -> IO CInt
sendFd (Socket (Fd fd)) (Fd fdToSend) = c_send_fd fd fdToSend
