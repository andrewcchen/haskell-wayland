{-# LANGUAGE DeriveDataTypeable #-}

module Graphics.Wayland.Internal.Registry
    ( WlId(..)
    , InternalId
    , InternalIdMap
    , newIdMap, idNew, idRemove, idGetInt, idGetExt
    , Object, wlObjId, wlObjDyn
    , Registry
    , regAdd, regDelete, regGet, regUpdate
    ) where

import Control.Exception.Base
import Data.Data
import Data.Dynamic
import Data.Maybe
import Data.Word
import qualified Data.Map.Lazy as M
import qualified Data.Map.Strict as MS

type InternalId = Word64
newtype WlId = WlId Word32
    deriving (Eq, Ord, Show, Typeable, Data)

data InternalIdMap = InternalIdMap Word64 (MS.Map WlId InternalId) (MS.Map InternalId WlId)

newIdMap :: Word64 -> InternalIdMap
newIdMap startId = InternalIdMap startId MS.empty MS.empty

idNew :: WlId -> InternalIdMap -> (InternalId, InternalIdMap)
idNew eId (InternalIdMap iId m1 m2) = (iId, InternalIdMap (iId+1) m1' m2')
    where m1' = assert (MS.notMember eId m1) $ MS.insert eId iId m1
          m2' = MS.insert iId eId m2

idRemove :: InternalId -> InternalIdMap -> InternalIdMap
idRemove iId (InternalIdMap x m1 m2) = InternalIdMap x m1' m2'
    where eId = fromJust $ MS.lookup iId m2
          m1' = MS.delete eId m1
          m2' = MS.delete iId m2

idGetInt :: WlId -> InternalIdMap -> InternalId
idGetInt eId (InternalIdMap _ m1 _) = fromJust $ MS.lookup eId m1

idGetExt :: InternalId -> InternalIdMap -> WlId
idGetExt iId (InternalIdMap _ _ m2) = fromJust $ MS.lookup iId m2

type Registry = M.Map InternalId Dynamic

class Object o where
    wlObjId :: o -> InternalId
    wlObjDyn :: o -> Dynamic

regAdd :: Object o => o -> Registry -> Registry
regAdd obj reg = assert (M.notMember oId reg) $ M.insert oId (wlObjDyn obj) reg
    where oId = wlObjId obj

regDelete :: Object o => o -> Registry -> Registry
regDelete obj reg = assert (M.member oId reg) $ M.delete oId reg
    where oId = wlObjId obj

regGet :: InternalId -> Registry -> Dynamic
regGet oId reg = assert (M.member oId reg) $ fromJust $ M.lookup oId reg

regUpdate :: Object o => o -> Registry -> Registry
regUpdate obj reg = assert (dynTypeRep (regGet oId reg) == dynTypeRep oDyn) $ M.insert oId oDyn reg
    where oId = wlObjId obj
          oDyn = wlObjDyn obj
