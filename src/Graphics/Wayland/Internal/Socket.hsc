#define _GNU_SOURCE
#include <sys/socket.h>

module Graphics.Wayland.Internal.Socket
    ( Socket, fdSocket
    , sockStream, sockNonblock, sockCloexec
    , socket, bind, connect, listen, accept, close
    , recv, send
    , read, write
    , recvFd, recvFdCloexec, sendFd
    , getPeerCred
    ) where

import Prelude hiding (read)

import Control.Exception (finally)
import Control.Monad (when)
import Data.Hashable (Hashable(..))
import Data.Int
import Data.Word
import Foreign.C.Error
import Foreign.C.String (CString, newCString)
import Foreign.C.Types
import Foreign.Marshal.Alloc (free, allocaBytes)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (peekByteOff)
import System.Posix.Types (Fd(..))

newtype Socket = Socket Fd
    deriving (Eq, Ord, Show)

instance Hashable Socket where
    hashWithSalt s (Socket (Fd (CInt fd))) = s `hashWithSalt` fd

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
        c_socket #{const AF_UNIX} sockType 0

foreign import ccall "string.h strlen"
    c_strlen :: CString -> IO CInt

foreign import ccall "bind_unix"
    c_bind_unix :: CInt -> CString -> CInt -> IO CInt

bind :: Socket -> String -> IO ()
bind (Socket (Fd fd)) path = do
    str <- newCString path
    len <- c_strlen str
    when (len >= 108) $ ioError $ userError $ "bind: path too long: "++show path
    finally (throwErrnoIfMinus1_ "bind" $ c_bind_unix fd str len)
            (free str)

foreign import ccall "connect_unix"
    c_connect_unix :: CInt -> CString -> CInt -> IO CInt

connect :: Socket -> String -> IO ()
connect (Socket (Fd fd)) path = do
    str <- newCString path
    len <- c_strlen str
    when (len >= 108) $ ioError $ userError $ "connect: path too long: "++show path
    finally (throwErrnoIfMinus1_ "connect" $ c_connect_unix fd str len)
            (free str)

foreign import ccall "sys/socket.h listen"
    c_listen :: CInt -> CInt -> IO CInt

listen :: Socket -> CInt -> IO ()
listen (Socket (Fd fd)) backlog =
    throwErrnoIfMinus1_ "listen" $ c_listen fd backlog

foreign import ccall "sys/socket.h accept4"
    c_accept4 :: CInt -> Ptr () -> Ptr () -> CInt -> IO CInt

accept :: Socket -> CInt -> IO Socket
accept (Socket (Fd fd)) flags = do
    newFd <- throwErrnoIfMinus1 "accept" $
        c_accept4 fd nullPtr nullPtr flags
    return $ Socket $ Fd newFd

foreign import ccall "unistd.h close"
    c_close :: CInt -> IO CInt

close :: Socket -> IO ()
close (Socket (Fd fd)) = throwErrnoIfMinus1_ "close" $ c_close fd

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
    c_recv_fd :: CInt -> CInt -> IO CInt

recvFd :: Socket -> IO CInt
recvFd (Socket (Fd fd)) = c_recv_fd fd 0

recvFdCloexec :: Socket -> IO CInt
recvFdCloexec (Socket (Fd fd)) = c_recv_fd fd #{const MSG_CMSG_CLOEXEC}

foreign import ccall "send_fd"
    c_send_fd :: CInt -> CInt -> IO CInt

sendFd :: Socket -> Fd -> IO CInt
sendFd (Socket (Fd fd)) (Fd fdToSend) = c_send_fd fd fdToSend

foreign import ccall "sys/socket.h getsockopt"
    c_getsockopt :: CInt -> CInt -> CInt -> Ptr () -> Ptr CInt -> IO CInt

-- Copied from Network.Socket
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/network/LICENSE)
getPeerCred :: Socket -> IO (CUInt, CUInt, CUInt)
getPeerCred (Socket (Fd fd)) = do
  let sz = (#const sizeof(struct ucred))
  allocaBytes sz $ \ ptr_cr ->
   with (fromIntegral sz) $ \ ptr_sz -> do
     throwErrnoIfMinus1Retry_ "getPeerCred" $
       c_getsockopt fd (#const SOL_SOCKET) (#const SO_PEERCRED) ptr_cr ptr_sz
     pid <- (#peek struct ucred, pid) ptr_cr
     uid <- (#peek struct ucred, uid) ptr_cr
     gid <- (#peek struct ucred, gid) ptr_cr
     return (pid, uid, gid)
