{-# LANGUAGE LambdaCase, RecordWildCards #-}

module Graphics.Wayland.Connection where

import Graphics.Wayland.Marshal
import Graphics.Wayland.Object
import Graphics.Wayland.Registry
--import qualified Graphics.Wayland.System.Socket as Sock

import Control.Monad.Except
import Control.Monad.ST
import Control.Monad.State.Lazy
import Data.Coerce
import Data.Dynamic
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import System.Posix.Types
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as MV
import Control.Lens
import Data.Hashable

class Connection c where
    connRecv :: c -> Ptr Word8 -> Int -> IO Int
    connSend :: c -> Ptr Word8 -> Int -> IO Int
    connRecvFd :: c -> IO Fd
    connSendFd :: c -> Fd -> IO ()

recvMessageHeader :: Connection c => c -> IO (Int, Int, Int)
recvMessageHeader conn = allocaBytesAligned 8 4 $ \p -> do
    cnt <- connRecv conn p 8
    when (cnt < 8) $ ioError $ userError $ "recvMessageHeader: incomplete read"
    sender <- fromIntegral <$> (peek $ p `plusPtr` 0 :: IO Word32)
    opcode <- fromIntegral <$> (peek $ p `plusPtr` 4 :: IO Word16)
    len <- fromIntegral <$> (peek $ p `plusPtr` 6 :: IO Word16)
    return (sender, opcode, len-8)

recvMessageBody :: Connection c => c -> Int -> IO (V.Vector Word32)
recvMessageBody conn len = do
    buf <- MV.unsafeNew (len `quot'` 4) :: IO (MV.MVector RealWorld Word32)
    MV.unsafeWith buf $ \p -> do
        cnt <- connRecv conn (castPtr p) len
        when (cnt < len) $ ioError $ userError $ "recvMessageBody: incomplete read"
        V.unsafeFreeze buf
    where quot' a b = (a+b-1) `quot` b

recvMessageFds :: Connection c => c -> Int -> IO [Fd]
recvMessageFds conn cnt = sequence $ take cnt $ repeat $ connRecvFd conn

--sendMessage :: Connection c => c -> InterfaceSpec -> WlId -> Int -> [Dynamic] -> IO Bool
--sendMessage sock spec sender opcode args = do
--    let func = view interfaceSendFuncs spec !! opcode
--        argTypes = map (view argType) $ view funcArgs func
--        len = 8 + (foldr (+) 0 $ map (uncurry sizeArg) $ zip argTypes args)
--    buf0 <- MV.new (len `quot` 4) :: IO (MV.MVector RealWorld Word32)
--    let buf4 = MV.unsafeCast (MV.tail buf0) :: MV.MVector RealWorld Word16
--        buf6 = MV.tail buf4
--        buf8 = MV.drop 2 buf0
--    MV.write buf0 0 $ coerce $ sender
--    MV.write buf4 0 $ fromIntegral opcode
--    MV.write buf6 0 $ fromIntegral len
--    flip evalStateT buf8 $ forM_ (zip argTypes args) $ \case
--        (ArgFd, arg) -> liftIO $ Sock.sendFd sock (fromDyn' arg) >> return ()
--        (typ, arg) -> writeArg typ arg >> return ()
--    ret <- MV.unsafeWith buf0 $ \p -> Sock.write sock (castPtr p) len
--    return $ ret == len
