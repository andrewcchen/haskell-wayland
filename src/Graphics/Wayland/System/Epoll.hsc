{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Graphics.Wayland.System.Epoll where

import Control.Exception.Base
import Data.Bits
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import System.Posix.IO
import System.Posix.Types

#include <sys/epoll.h>

data Device = Device Fd Int (Ptr Event)

newtype CreateFlags = CreateFlags CInt
pattern Cloexec :: CreateFlags
pattern Cloexec = CreateFlags #{const EPOLL_CLOEXEC}

foreign import ccall unsafe "epoll_create1"
    epoll_create1 :: CInt -> IO CInt

create :: [CreateFlags] -> Int -> IO Device
create flags maxevs = do
    fd <- throwErrnoIfMinus1 "epoll_create1" $ epoll_create1 fl
    ptr <- mallocArray maxevs
    return $ Device (Fd fd) maxevs ptr
    where fl = foldr (\(CreateFlags x) a -> x .|. a) 0 flags

close :: Device -> IO ()
close (Device fd _ ptr) = do
    closeFd fd
    free ptr

newtype EventType = EventType CInt
pattern InEvent :: EventType
pattern InEvent = EventType #{const EPOLLIN}
pattern OutEvent :: EventType
pattern OutEvent = EventType #{const EPOLLOUT}
pattern ErrorEvent :: EventType
pattern ErrorEvent = EventType #{const EPOLLERR}
pattern HangupEvent :: EventType
pattern HangupEvent = EventType #{const EPOLLHUP}

foreign import ccall unsafe "epoll_ctl"
    epoll_ctl :: CInt -> CInt -> CInt -> Ptr Event -> IO CInt

add :: Device -> [EventType] -> Fd -> IO ()
add (Device (Fd epfd) _ _) events (Fd fd) = bracket
    (do ptr <- mallocBytes #{size struct epoll_event}
        poke ptr $ Event (Fd fd) events
        return ptr)
    free
    (\p -> throwErrnoIfMinus1_ "epoll_ctl" $
            epoll_ctl epfd #{const EPOLL_CTL_ADD} fd p)

delete :: Device -> Fd -> IO ()
delete (Device (Fd epfd) _ _) (Fd fd) = throwErrnoIfMinus1_ "epoll_ctl" $
    epoll_ctl epfd #{const EPOLL_CTL_DEL} fd nullPtr

data Event = Event
    { eventFd :: !Fd
    , eventTypes :: [EventType]
    }

decodeEventTypes :: CInt -> [EventType]
decodeEventTypes !x = filter (\(EventType e) -> x .&. e /= 0) possibleEvents
    where possibleEvents = [InEvent, OutEvent, HangupEvent, ErrorEvent]

encoodeEventTypes :: [EventType] -> CInt
encoodeEventTypes evs = foldr (\(EventType e) a -> e .|. a) 0 evs

instance Storable Event where
    sizeOf _ = #{size struct epoll_event}
    alignment _ = #{alignment struct epoll_event}
    peek ptr = do
        eventTypes <- decodeEventTypes <$> #{peek struct epoll_event, events} ptr
        eventFd <- #{peek struct epoll_event, data} ptr
        return Event{..}
    poke ptr (Event (Fd fd) evs) = do
        #{poke struct epoll_event, events} ptr $ encoodeEventTypes evs
        #{poke struct epoll_event, data} ptr fd

foreign import ccall safe "epoll_wait"
    epoll_wait_safe :: CInt -> Ptr Event -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "epoll_wait"
    epoll_wait_unsafe :: CInt -> Ptr Event -> CInt -> CInt -> IO CInt

waitNonblock :: Device -> IO [Event]
waitNonblock (Device (Fd epfd) maxevs ptr) = do
    cnt <- throwErrnoIfMinus1 "epoll_wait" $
            epoll_wait_unsafe epfd ptr (fromIntegral maxevs) 0
    peekArray (fromIntegral cnt) ptr

wait :: Device -> Int -> IO [Event]
wait = undefined -- TODO we have to sort out signals
