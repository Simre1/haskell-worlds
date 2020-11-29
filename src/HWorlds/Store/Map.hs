{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HWorlds.Store.Map where

import HWorlds.Core
import qualified Data.HashTable.IO as H
import qualified Data.IntSet as S

hashWorld :: forall a. IO (World (Init :> Get a :> Set a :> Destroy a :> Member a))
hashWorld = do
  ref <- H.new :: IO (H.LinearHashTable Int a)
  pure $ emptyWorld 
    `addCapability` CapGet (\(Entity i) -> H.lookup ref i)
    `addCapability` CapSet (\(Entity i) a -> H.insert ref i a)
    `addCapability` CapDestroy (\(Entity i) -> H.delete ref i)
    `addCapability` CapMember (Entities <$> H.foldM (\a (b,_) -> pure $ S.insert b a) S.empty ref)
