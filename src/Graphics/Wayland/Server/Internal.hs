{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Graphics.Wayland.Server.Internal
    ( module Graphics.Wayland.Internal
    , MonadWayland, getStateRef, runWithState, extractState, injectState
    , WaylandStateRef, newStateRef
    , InterfaceImplementation, wrapImplementation
    , wrapCallback, wrapCall
    , (.:), (.::), (.:::), (.::::), (.:::::), (.::::::), (.:::::::)
    , Pointers, freePointers
    , WaylandInterface, wlInterface, wlDispatcher
    , WlInterface(..), WlResource(..), WlClient(..)
    ) where

import Control.Monad.IO.Class
import Data.IORef
import Data.Maybe
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.StablePtr
import System.IO.Unsafe
import Data.Data
import Data.Word
import Data.Int


import Graphics.Wayland.Internal

newtype WaylandStateRef s = WaylandStateRef (IORef (Maybe s))

newStateRef :: IO (WaylandStateRef s)
newStateRef = fmap WaylandStateRef $ newIORef Nothing

class (Monad m, MonadIO m) => MonadWayland s m | m -> s where
    getStateRef :: m (WaylandStateRef s)
    runWithState :: m a -> s -> IO (a, s)
    extractState :: m s
    injectState :: s -> m ()

{-# NOINLINE nullStateRef #-}
nullStateRef :: WaylandStateRef ()
nullStateRef = unsafePerformIO $ newStateRef

instance MonadWayland () IO where
    getStateRef = return nullStateRef
    runWithState m s = fmap (,s) m
    extractState = return ()
    injectState _ = return ()

readIORefWithCheck :: IORef (Maybe a) -> IO a
readIORefWithCheck ref = readIORef ref >>= \case
    Nothing -> error "wayland: stored state empty on read. THIS SHOULD NOT HAPPEN"
    Just state -> writeIORef ref Nothing >> return state

writeIORefWithCheck :: IORef (Maybe a) -> a -> IO ()
writeIORefWithCheck ref x = readIORef ref >>= \case
    Just _ -> error "wayland: stored state full on write. THIS SHOULD NOT HAPPEN"
    Nothing -> writeIORef ref $ Just x

wrapCallback :: MonadWayland s m => WaylandStateRef s -> m a -> IO a
wrapCallback (WaylandStateRef stateRef) f = do
    state <- liftIO $ readIORefWithCheck stateRef
    (x, newState) <- runWithState f state
    liftIO (writeIORefWithCheck stateRef newState)
    return x

wrapCall :: MonadWayland s m => IO a -> m a
wrapCall f = do
    WaylandStateRef stateRef <- getStateRef
    extractState >>= liftIO . writeIORefWithCheck stateRef
    x <- liftIO f
    liftIO (readIORefWithCheck stateRef) >>= injectState
    return x


newtype WlInterface = WlInterface (Ptr WlInterface) deriving (Eq, Ord, Show, Typeable, Data)

newtype WlResource = WlResource (Ptr WlResource) deriving (Eq, Ord, Show, Typeable, Data)

newtype WlClient = WlClient (Ptr WlClient) deriving (Eq, Ord, Show, Typeable, Data)

class WaylandInterface i where
    wlInterface :: i -> WlInterface
    wlDispatcher :: i -> WlResource -> Word32 -> Ptr () -> IO ()






class InterfaceImplementation i m where
    wrapImplementation :: i -> m [FunPtr ()]


type Pointers = ([Ptr ()], [FunPtr ()])

freePointers :: Ptr () -- | must be a 'Ptr' cast from a 'StablePtr Pointers'
             -> IO ()
freePointers pRef = do
    let spRef = castPtrToStablePtr pRef
    (ptrs, funptrs) <- deRefStablePtr spRef {- >>= readIORef -} :: IO Pointers
    mapM_ free ptrs
    mapM_ freeHaskellFunPtr funptrs
    freeStablePtr spRef




(.:) = (.).(.)
(.::) = (.).(.).(.)
(.:::) = (.).(.).(.).(.)
(.::::) = (.).(.).(.).(.).(.)
(.:::::) = (.).(.).(.).(.).(.).(.)
(.::::::) = (.).(.).(.).(.).(.).(.).(.)
(.:::::::) = (.).(.).(.).(.).(.).(.).(.).(.)

infixr 9 .:
infixr 9 .::
infixr 9 .:::
infixr 9 .::::
infixr 9 .:::::
infixr 9 .::::::
infixr 9 .:::::::
