{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HWorlds.Store.HashMap where

import HWorlds.Core
import qualified Data.HashTable.IO as H
import qualified Data.IntMap as IM

hashWorld :: forall a. IO (World (Init :> Get a :> Set a :> Destroy a :> Member a))
hashWorld = do
  ref <- H.new :: IO (H.LinearHashTable Int a)
  pure $ emptyWorld 
    `addCapability` CapGet (\(Entity i) -> H.lookup ref i)
    `addCapability` CapSet (\(Entity i) a -> H.insert ref i a)
    `addCapability` CapDestroy (\(Entity i) -> H.delete ref i)
    -- `addCapability` CapMember (Entities <$> hashtableToStream ref)
    `addCapability` CapMember (Entities <$> H.foldM (\s (eId, a) -> pure $ IM.insert eId a s) IM.empty ref)

-- hashtableToStream :: H.LinearHashTable Int a -> IO (S.SerialT m (Entity, a))
-- hashtableToStream = H.foldM (\s (eId, a) -> pure $! S.cons (Entity eId, a) s) S.nil