{-# LANGUAGE KindSignatures, RankNTypes, RecordWildCards, TemplateHaskell #-}

module Graphics.Wayland.Internal.Registry
    ( WlId(..)
    , Registry
    , newRegistry
    ) where

import Graphics.Wayland.Internal.Marshal
--import Graphics.Wayland.Internal.Dynamic
import Graphics.Wayland.Internal.Object

--import Control.Exception.Base
--import Data.Data
import Control.Exception
import Data.Coerce
import Data.Maybe
import Data.Word
--import qualified Data.Map.Lazy as M
--import qualified Data.Map.Strict as MS
import qualified Data.Vector.Mutable as MV
import Control.Lens
import Data.IORef
import Control.Monad.ST (RealWorld)
import Control.Monad
import Unsafe.Coerce
import GHC.Prim
import qualified Data.HashTable.IO as H
import Data.Dynamic

import qualified Data.IntMap.Lazy as M


data RegistryError = ObjectDoesNotExist Word32
                   | ObjectAlreadyExists Word32
    deriving (Show)
instance Exception RegistryError



data Registry = ServerRegistry {-# UNPACK #-} (IORef RegState)
              | ClientRegistry {-# UNPACK #-} (IORef RegState)

regRef :: Registry -> IORef RegState
regRef ServerRegistry ref = ref
regRef ClientRegistry ref = ref

data RegState = RegState
    { nextId :: Word32
    , freeIds :: [Word32]
    , objs :: M.IntMap Object
    }




regGet :: Registry -> Word32 -> IO Object
regGet reg i = do
    os <- objs <$> readIORef (regRef reg)
    maybe (throwIO $ ObjectDoesNotExist i) return $ M.lookup i os

regDel :: Registry -> Object -> IO ()
regDel reg obj = atomicModifyIORef ref $ \r -> do
    let i = objId obj
    when (M.notMember i $ objs r) $ throwIO $ ObjectDoesNotExist i
    return (r { freeIds = i : freeIds r
              , objs = M.delete i $ objs r
              }, ())
    where ref = regRef reg

regAdd :: Registry -> Object -> IO ()
regAdd reg obj = atomicModifyIORef (regRef reg) $ \r -> do
    let i = objId obj
    when (M.member i $ objs r) $ throwIO $ ObjectAlreadyExists i
    return (r { objs = M.add i $ objs r }, ())

regNew :: Registry -> InterfaceSpec -> IO Object
regNew typ reg@(ServerRegistry ref) = atomicModifyIORef ref $ \r ->
    if null (freeIds r) then do
        o <- newObject typ (nextId r)
        r { nextId = nextId r + 1 } regAdd kek




--data Registry = Registry
--    { _remoteIdBase :: Int
--    , _localIdBase :: Int
--    , _localFreeList :: [Int]
--    , _localNextId :: Int
--    , _remoteNextId :: Int
--    , _localTypes :: MV.MVector RealWorld (Maybe TypeRep)
--    , _remoteTypes :: MV.MVector RealWorld (Maybe TypeRep)
--    , _localObjs :: MV.MVector RealWorld Any
--    , _remoteObjs :: MV.MVector RealWorld Any
--    }
data Registry = Registry
    { _remoteIdBase :: WlId
    , _localIdBase :: WlId
    , _localFreeList :: [WlId]
    , _localNextId :: Word32
    , _types :: H.BasicHashTable WlId TypeRep
    , _objs :: H.BasicHashTable WlId Object
    }
makeLenses ''Registry

newRegistry :: Word32 -> Word32 -> IO (IORef Registry)
newRegistry rBase lBase = assert (rBase /= lBase) $ do
    let _remoteIdBase = WlId rBase
        _localIdBase = WlId lBase
        _localFreeList = []
        _localNextId = lBase
    _types <- H.new
    _objs <- H.new
    newIORef Registry{..}

newId :: IORef Registry -> IO WlId
newId ref = do
    reg <- readIORef ref
    if null $ reg^.localFreeList
       then do let x = WlId $ reg^.localNextId
               modifyIORef' ref $ over localNextId (+1)
               return x
       else do let x = head $ reg^.localFreeList
               modifyIORef' ref $ over localFreeList tail
               return x

idIsLocal :: Registry -> WlId -> Bool
idIsLocal reg i = (reg^.localIdBase > reg^.remoteIdBase && i >= reg^.localIdBase)
               || (reg^.remoteIdBase > reg^.localIdBase && i < reg^.remoteIdBase)

deleteObj :: IORef Registry -> Object -> IO ()
deleteObj ref o = do
    reg <- readIORef ref
    let i = objId o
    x <- H.lookup (reg^.types) i
    when (isNothing x) $ ioError $ userError $ "id does not exist"
    H.delete (reg^.types) i
    H.delete (reg^.objs) i
    when (idIsLocal reg i) $ modifyIORef ref $ over localFreeList (i:)

reserveObj :: IORef Registry -> WlId -> TypeRep -> IO ()
reserveObj ref i typ = do
    reg <- readIORef ref
    x <- H.lookup (reg^.types) i
    when (isJust x) $ ioError $ userError $ "new id already exists"
    H.insert (reg^.types) i typ

unsafeInsertObj :: IORef Registry -> Object -> IO ()
unsafeInsertObj ref o = do
    reg <- readIORef ref
    let i = objId o
        typ = objType o
    x <- H.lookup (reg^.objs) i
    when (isJust x) $ ioError $ userError $ "object already exists"
    H.insert (reg^.types) i typ
    H.insert (reg^.objs) i o

insertRemoteObj :: IORef Registry -> Object -> IO ()
insertRemoteObj ref o = do
    reg <- readIORef ref
    let i = objId o
        typ = objType o
    x <- H.lookup (reg^.types) i
    when (isNothing x) $ ioError $ userError $ "object has not been reserved"
    when (fromJust x /= typ) $ ioError $ userError $ "object type does not match with reserved type"
    y <- H.lookup (reg^.objs) i
    when (isJust y) $ ioError $ userError $ "object already exists"
    H.insert (reg^.objs) i o

getObj :: IORef Registry -> WlId -> IO Object
getObj ref i = do
    reg <- readIORef ref
    x <- H.lookup (reg^.objs) i
    when (isNothing x) $ ioError $ userError $ "object does not exist"
    return $ fromJust x

--reserveRemoteObj :: IORef Registry -> WlId -> TypeRep -> IO ()
--reserveRemoteObj ref x typ = do
--    reg <- readIORef ref
--    let i = (fromIntegral (coerce x :: Word32)) - reg ^. remoteIdBase
--    when (i > reg ^. remoteNextId) $ ioError $ userError $ "new id must be sequential: "++show i
--    when (i == reg ^. remoteNextId) $ modifyIORef' ref $ over remoteNextId (+1)
--    reg' <- readIORef ref
--    when (i == MV.length (reg' ^. remoteObjs)) $ do
--        v' <- grow' (reg' ^. remoteObjs) $ unsafeCoerce NullObj
--        writeIORef ref $ set remoteObjs v' reg'
--    reg'' <- readIORef ref
--    ex <- MV.read (reg'' ^. remoteTypes) i
--    when (not $ isNothing ex) $ ioError $ userError $ "new id is already assigned: "++show i
--    MV.write (reg'' ^. remoteObjs) i $ unsafeCoerce NullObj
--    MV.write (reg'' ^. remoteTypes) i $ Just typ
--
--assignRemoteObj :: IORef Registry -> WlId -> Dynamic -> IO ()
--assignRemoteObj ref x obj = do
--    reg <- readIORef ref
--    let i = (fromIntegral (coerce x :: Word32)) - reg ^. remoteIdBase
--    typ <- MV.read (reg ^. remoteTypes) i
--    when (not $ isNothing typ) $ ioError $ userError $ "new id has not been reserved: "++show i
--    when (fromJust typ /= dynTypeRep obj) $ ioError $ userError $ "object type does not match with reserved type"
--    MV.write (reg ^. remoteObjs) i $ dynToObj obj
--
--deleteRemoteObj :: IORef Registry -> WlId -> IO ()
--deleteRemoteObj ref x = do
--    reg <- readIORef ref
--    let i = (fromIntegral (coerce x :: Word32)) - reg ^. remoteIdBase
--    MV.write (reg ^. remoteTypes) i Nothing
--    MV.write (reg ^. remoteObjs) i $ unsafeCoerce NullObj
--
--getDyn :: IORef Registry -> WlId -> IO Dynamic
--getDyn ref x = do
--    reg <- readIORef ref
--    let i = (fromIntegral (coerce x :: Word32)) - reg ^. remoteIdBase
--    let (typs,objs) = if (reg^.localIdBase > reg^.remoteIdBase && i < reg^.localIdBase) ||
--                         (reg^.remoteIdBase > reg^.localIdBase && i >= reg^.remoteIdBase)
--                         then (reg^.remoteTypes,reg^.remoteObjs)
--                         else (reg^.localTypes,reg^.localObjs)
--
--    typ <- MV.read typs i
--    when (isNothing typ) $ ioError $ userError $ "invalid object id: "++show i
--    obj <- MV.read objs i
--    return $ Dynamic (fromJust typ) (unsafeCoerce obj)
--
--getObj :: Object o => IORef Registry -> WlId -> IO o
--getObj ref x = do
--    Dynamic _ o <- getDyn ref x
--    return $ unsafeCoerce o
--
--
--type InternalId = Word64
--
--newtype WlId = WlId Word32
--    deriving (Eq, Ord, Show)
--
--data InternalIdMap = InternalIdMap Word64 (MS.Map WlId InternalId) (MS.Map InternalId WlId)
--
--newIdMap :: Word64 -> InternalIdMap
--newIdMap startId = InternalIdMap startId MS.empty MS.empty
--
--idNew :: WlId -> InternalIdMap -> (InternalId, InternalIdMap)
--idNew eId (InternalIdMap iId m1 m2) = (iId, InternalIdMap (iId+1) m1' m2')
--    where m1' = assert (MS.notMember eId m1) $ MS.insert eId iId m1
--          m2' = MS.insert iId eId m2
--
--idRemove :: InternalId -> InternalIdMap -> InternalIdMap
--idRemove iId (InternalIdMap x m1 m2) = InternalIdMap x m1' m2'
--    where eId = fromJust $ MS.lookup iId m2
--          m1' = MS.delete eId m1
--          m2' = MS.delete iId m2
--
--idGetInt :: InternalIdMap -> WlId -> Maybe InternalId
--idGetInt (InternalIdMap _ m1 _) eId = MS.lookup eId m1
--
--idGetExt :: InternalIdMap -> InternalId -> WlId
--idGetExt (InternalIdMap _ _ m2) iId = fromJust $ MS.lookup iId m2
--
--type Registry = M.Map InternalId Dynamic
--
--regAdd :: Object o => Registry -> o -> Registry
--regAdd reg obj = assert (M.notMember oId reg) $ M.insert oId (objDyn obj) reg
--    where oId = objId obj
--
--regDelete :: Object o => Registry -> o -> Registry
--regDelete reg obj = assert (M.member oId reg) $ M.delete oId reg
--    where oId = objId obj
--
--regGet :: Registry -> InternalId -> Dynamic
--regGet reg oId = assert (M.member oId reg) $ fromJust $ M.lookup oId reg
--
--regUpdate :: Object o => Registry -> o -> Registry
--regUpdate reg obj = assert (dynTypeRep (regGet oId reg) == dynTypeRep oDyn) $ M.insert oId oDyn reg
--    where oId = objId obj
--          oDyn = objDyn obj
