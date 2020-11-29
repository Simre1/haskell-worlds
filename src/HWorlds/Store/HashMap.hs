{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HWorlds.Store.HashMap where

import HWorlds.Core
import qualified Data.IntMap as M
import Data.IORef ( modifyIORef, newIORef, readIORef )

mapWorld :: forall a. IO (World (Init :> Get a :> Set a :> Destroy a :> Member a))
mapWorld = do
  ref <- newIORef M.empty
  pure $ emptyWorld 
    `addCapability` CapGet (\(Entity i) -> M.lookup i <$> readIORef ref)
    `addCapability` CapSet (\(Entity i) a -> modifyIORef ref (M.insert i a))
    `addCapability` CapDestroy (\(Entity i) -> modifyIORef ref (M.delete i))
    `addCapability` CapMember (Entities . M.keysSet <$> readIORef ref)
