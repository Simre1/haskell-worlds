{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HWorlds.Store.Map where

import HWorlds.Core
import qualified Data.IntMap.Strict as M
import Data.IORef ( modifyIORef, newIORef, readIORef )

mapWorld :: forall a. IO (World (Init :> Get a :> Set a :> Destroy a :> Member a))
mapWorld = do
  ref <- newIORef M.empty
  pure $ emptyWorld 
    `addCapability` CapGet (\(Entity i) -> M.lookup i <$> readIORef ref)
    `addCapability` CapSet (\(Entity i) a -> modifyIORef ref (M.insert i a))
    `addCapability` CapDestroy (\(Entity i) -> modifyIORef ref (M.delete i))
    `addCapability` CapMember (Entities <$> readIORef ref)

-- mapToStream :: M.IntMap a -> S.SerialT m (Entity, a)
-- mapToStream = M.foldlWithKey' (\s eId a -> id $! S.cons (Entity eId, a) s) S.nil