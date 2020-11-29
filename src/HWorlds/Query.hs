{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}

module HWorlds.Query where

import HWorlds.Core
import Control.Monad.Trans.Reader ( ask, ReaderT(..) )
import Control.Monad.IO.Class ( MonadIO )
import Control.Monad.Trans.Class ( MonadTrans(..) )
import Data.Proxy ( Proxy(..) )
import Data.Maybe (fromJust)
import Control.Applicative ( Applicative(liftA2) )
import Data.Monoid ( Dual(Dual, getDual), Endo(Endo, appEndo) )
import Data.Coerce ( coerce, Coercible )
import Control.Monad ((>=>))

executeQuery :: forall i as m s. (MultiMember as i, MultiGet as i, Monoid s, MonadIO m) =>
  (i -> QueryBody as m s) -> WorldT as m s
executeQuery makeQuery = do
  entities <- multiMember (Proxy @i)
  eFoldMap entities $ \e -> do
    i <- multiGet e
    case i of
      Nothing -> error "Members accessed that do not have this entity"
      Just i -> do
        let (QueryBody readerT) = makeQuery i
        runReaderT readerT e

newtype QueryBody as m a = QueryBody (ReaderT Entity (WorldT as m) a) deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans (QueryBody as) where
  lift m = QueryBody $ ReaderT $ \_ -> lift m
  {-# INLINE lift #-}

liftWorldT :: WorldT as m a -> QueryBody as m a
liftWorldT w = QueryBody $ ReaderT $ const w
{-# INLINE liftWorldT #-}

queriedEntity :: Monad m => QueryBody as m Entity
queriedEntity = QueryBody ask

qSet :: (MonadIO m, MultiSet as a) => a -> QueryBody as m ()
qSet a = queriedEntity >>= liftWorldT . flip multiSet a

qDestroy :: forall a as m. (MonadIO m, MultiDestroy as a) => Proxy a -> QueryBody as m ()
qDestroy p = queriedEntity >>= liftWorldT . flip multiDestroy p

cmap :: (MultiGet as a, MultiMember as a, MonadIO m, MultiSet as b) => (a -> b) -> WorldT as m ()
cmap f = executeQuery $ qSet . f

cmapM :: (MultiGet as a, MultiMember as a, MonadIO m, MultiSet as b) => (a -> QueryBody as m b) -> WorldT as m ()
cmapM f = executeQuery $ f >=> qSet

cmapM_ :: (MultiGet as a, MultiMember as a, MonadIO m) => (a -> QueryBody as m ()) -> WorldT as m ()
cmapM_ f = executeQuery $ \i -> f i

cfold :: (MultiGet as a, MultiMember as a, MonadIO m, Monoid b) => (a -> b) -> WorldT as m b
cfold f = executeQuery $ pure . f

cfoldr :: (MultiGet as a, MultiMember as a, MonadIO m) => (a -> b -> b) -> b -> WorldT as m b
cfoldr f b = fmap (($b) . appEndo) $ cfold $ Endo #. f

cfoldl :: (MultiGet as a, MultiMember as a, MonadIO m) => (b -> a -> b) -> b -> WorldT as m b
cfoldl f b = fmap (($b) . appEndo . getDual) $ cfold $ Dual . Endo . flip f

class QueryHead' (b :: Bool) (as :: AppendList Op) is where
  getQueryHead :: MonadIO m => Proxy b -> Entity -> WorldT as m is
  getQueryEntities :: MonadIO m => Proxy b -> Proxy is -> WorldT as m Entities

instance (HasCapability as (Get a), HasCapability as (Member a)) => QueryHead' True as a where
  getQueryHead _ e = fromJust <$> eGet e
  getQueryEntities _ _ = eMember (Proxy @a)
  {-# INLINE getQueryHead #-}
  {-# INLINE getQueryEntities #-}

type QueryHeadContains as i = (And (Contains as (Get i)) (Contains as (Member i)))

type QueryHead as a = QueryHead' (QueryHeadContains as a) as a

instance (QueryHead as a, QueryHead as b) => QueryHead' False as (a,b) where
  getQueryHead _ e = (,) <$> getQueryHead (Proxy @(QueryHeadContains as a)) e <*> getQueryHead (Proxy @(QueryHeadContains as b)) e
  getQueryEntities _ _ = intersectEntities <$> getQueryEntities (Proxy @(QueryHeadContains as a)) (Proxy @a) <*> getQueryEntities (Proxy @(QueryHeadContains as b)) (Proxy @b)
  {-# INLINE getQueryHead #-}
  {-# INLINE getQueryEntities #-}

instance (QueryHead as a, QueryHead as b, QueryHead as c) => QueryHead' False as (a,b,c) where
  getQueryHead _ e = (,,) <$> getQueryHead (Proxy @(QueryHeadContains as a)) e <*> getQueryHead (Proxy @(QueryHeadContains as b)) e <*> getQueryHead (Proxy @(QueryHeadContains as c)) e
  getQueryEntities _ _ = liftA2 intersectEntities (getQueryEntities (Proxy @(QueryHeadContains as a)) (Proxy @a)) $
    liftA2 intersectEntities (getQueryEntities (Proxy @(QueryHeadContains as b)) (Proxy @b)) (getQueryEntities (Proxy @(QueryHeadContains as c)) (Proxy @c))
  {-# INLINE getQueryHead #-}
  {-# INLINE getQueryEntities #-}

instance (QueryHead' (QueryHeadContains as a) as a, QueryHead' (QueryHeadContains as b) as b, QueryHead' (QueryHeadContains as c) as c, QueryHead' (QueryHeadContains as d) as d) => QueryHead' False as (a,b,c,d) where
  getQueryHead _ e = (,,,) <$> getQueryHead (Proxy @(QueryHeadContains as a)) e <*> getQueryHead (Proxy @(QueryHeadContains as b)) e <*> getQueryHead (Proxy @(QueryHeadContains as c)) e <*> getQueryHead (Proxy @(QueryHeadContains as d)) e
  getQueryEntities _ _ = liftA2 intersectEntities
    (liftA2 intersectEntities (getQueryEntities (Proxy @(QueryHeadContains as a)) (Proxy @a)) (getQueryEntities (Proxy @(QueryHeadContains as b)) (Proxy @b)))
    (liftA2 intersectEntities (getQueryEntities (Proxy @(QueryHeadContains as c)) (Proxy @c)) (getQueryEntities (Proxy @(QueryHeadContains as d)) (Proxy @d)))
  {-# INLINE getQueryHead #-}
  {-# INLINE getQueryEntities #-}

-- See Data.Foldable
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce
{-# INLINE (#.) #-}