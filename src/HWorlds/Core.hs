{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}

module HWorlds.Core where

import qualified Data.Vector as V

import Data.IORef

import Data.Void ( Void )
import Unsafe.Coerce (unsafeCoerce)
import Data.Proxy
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
-- import qualified Data.IntSet as S
import Data.Maybe
import Data.Bool
import GHC.TypeLits
import Control.Applicative
import Data.Functor.Identity
import Data.Foldable
import qualified Data.IntMap.Strict as IM

newtype Entity = Entity Int deriving (Eq, Show, Ord)

newtype Entities a = Entities (IM.IntMap a) deriving Show

intersectEntities :: (a -> b -> c) -> Entities a -> Entities b -> Entities c
intersectEntities f (Entities m1) (Entities m2) = Entities $ IM.intersectionWith f m1 m2

-- Turned out be slower!
-- data Entities a = Entities (S.SerialT Identity (Entity, a)) --(IM.IntMap a) deriving Show

-- Entity Ids must be ordered highest to lowest!!!
-- intersectEntities :: forall a b c. (a -> b -> c) -> Entities a -> Entities b -> Entities c
-- intersectEntities f (Entities !es1) (Entities !es2) = Entities . runIdentity $ go es1 es2
--   where 
--     go :: S.SerialT Identity (Entity, a) -> S.SerialT Identity (Entity, b) -> Identity (S.SerialT Identity (Entity, c))
--     go !s1 !s2 = do
--       maybe1 <- S.uncons s1
--       maybe2 <- S.uncons s2
--       case (maybe1,maybe2) of
--         (Just (!(e1,c1), rest1), Just (!(e2,c2), rest2)) -> if
--           | e1 < e2 -> id $! go s1 (S.dropWhile (\(e,_)  -> e1<e) rest2)
--           | e1 > e2 -> id $! go (S.dropWhile (\(e,_) -> e2<e) rest1) s2
--           | otherwise -> S.cons (e1,f c1 c2) <$> go rest1 rest2 
--         _ -> pure S.nil

newtype World (a :: AppendList Op) = World WorldStorage

mergeWorld :: World as -> World bs -> World (Merge as bs)
mergeWorld (World s1) (World s2) = World $ storageMerge s1 s2

emptyWorld :: World Init
emptyWorld = World emptyStorage

newtype WorldStorage = WorldStorage (V.Vector Void)

storageAdd :: Capability a -> WorldStorage -> WorldStorage
storageAdd c (WorldStorage s) = WorldStorage (V.snoc s (unsafeCoerce c))

storageGet :: Int -> WorldStorage -> Capability a
storageGet i (WorldStorage s) = unsafeCoerce $ s V.! i

storageMerge :: WorldStorage -> WorldStorage -> WorldStorage
storageMerge (WorldStorage s1) (WorldStorage s2) = WorldStorage $ s1 V.++ s2

emptyStorage :: WorldStorage
emptyStorage = WorldStorage V.empty

data Op where
  Get :: a -> Op
  Set :: a -> Op
  Destroy :: a -> Op
  Member :: a -> Op

data Capability (c :: Op) where
  CapGet :: (Entity -> IO (Maybe a)) -> Capability (Get a)
  CapSet :: (Entity -> a -> IO ()) -> Capability (Set a)
  CapDestroy :: (Entity -> IO ()) -> Capability (Destroy a)
  CapMember :: IO (Entities a) -> Capability (Member a)

addCapability :: World as -> Capability a -> World (as :> a)
addCapability (World v) component = World (storageAdd component v)

getCapability :: forall a as. HasCapability as a => World as -> Capability a
getCapability (World s) = flip storageGet s $ getIndex (Proxy @as) (Proxy @a)

newtype WorldT cs m a = WorldT (ReaderT (World cs) m a) deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

instance (Semigroup a, Applicative m) => Semigroup (WorldT cs m a) where
  (WorldT reader1) <> (WorldT reader2) = WorldT $ ReaderT $ \a -> 
    (<>) <$> runReaderT reader1 a <*> runReaderT reader2 a

instance (Monoid a, Applicative m) => Monoid (WorldT cs m a) where
  mempty = pure mempty

runWorldT :: World as -> WorldT as m a -> m a
runWorldT w (WorldT reader) = runReaderT reader w

askWorld :: Monad m => WorldT cs m (World cs)
askWorld = WorldT ask

eGet :: forall a as m. (HasCapability as (Get a), MonadIO m) => Entity -> WorldT as m (Maybe a)
eGet e = askWorld >>= \w -> case getCapability w :: Capability (Get a) of
  CapGet f -> liftIO $ f e

eSet :: forall a as m. (HasCapability as (Set a), MonadIO m) => Entity -> a -> WorldT as m ()
eSet e a = askWorld >>= \w -> case getCapability w :: Capability (Set a) of
  CapSet f -> liftIO $ f e a

eDestroy :: forall a as m. (HasCapability as (Destroy a), MonadIO m) => Entity -> Proxy a -> WorldT as m ()
eDestroy e _ = askWorld >>= \w -> case getCapability w :: Capability (Destroy a) of
  CapDestroy f -> liftIO $ f e

eMember :: forall a as m. (HasCapability as (Member a), MonadIO m) => Proxy a -> WorldT as m (Entities a)
eMember _ = askWorld >>= \w -> case getCapability w :: Capability (Member a) of
  CapMember f -> liftIO f

newtype NewEntity = NewEntity {fromNewEntity :: Entity}

entityWorld :: IO (World (Init :> Get NewEntity))
entityWorld = do
  ref <- newIORef (0 :: Int)
  pure $ emptyWorld
    `addCapability` CapGet (\_ -> atomicModifyIORef' ref (\a -> (succ a, Just $ NewEntity $ Entity a)))

newEntity :: (HasCapability as (Get NewEntity), MonadIO m) => WorldT as m Entity
newEntity = fromNewEntity . fromJust <$> eGet global

initEntity :: (HasCapability as (Get NewEntity), MonadIO m, MultiSet as a) => a -> WorldT as m Entity
initEntity s = do
  e <- newEntity
  multiSet e s
  pure e

global :: Entity
global = Entity (-1)

eFold :: (Monoid m) => Entities a -> ((Entity, a) -> m) -> m
eFold (Entities es) f = IM.foldMapWithKey (\k a -> f (Entity k, a)) es 
  -- runIdentity $ S.fold (S.foldMap f) es

morphWorldT :: Monad g => (forall x. f x -> g x) -> WorldT as f a -> WorldT as g a
morphWorldT f worldT = askWorld >>= \w -> lift (f (runWorldT w worldT))
{-# INLINE morphWorldT #-}

multiGet :: forall as a m. (MultiGet as a, MonadIO m) => Entity -> WorldT as m (Maybe a)
multiGet = multiGet' (Proxy @(Gettable as a))

multiSet :: forall as a m. (MultiSet as a, MonadIO m) => Entity -> a -> WorldT as m ()
multiSet = multiSet' (Proxy @(Settable as a))

multiDestroy :: forall as a m. (MultiDestroy as a, MonadIO m) => Entity -> Proxy a -> WorldT as m ()
multiDestroy = multiDestroy' (Proxy @(Destroyable as a))

multiMember :: forall as a m. (MultiMember as a, MonadIO m) => Proxy a -> WorldT as m (Entities a)
multiMember = multiMember' (Proxy @(Memberable as a))

class HasCapability (as :: AppendList Op) (a :: Op) where
  getIndex :: Proxy as -> Proxy a -> Int

instance HasCapability (Init :> a) a where
  getIndex _ _ = 0
  {-# INLINE getIndex #-}

instance (BoolSingleton (Equal x a), HasCapability (c :> b) (If (Equal x a) b a)) => HasCapability (c :> b :> x) a where
  getIndex _ _ = bool id succ (getBoolType (Proxy @(Equal x a))) $ getIndex (Proxy :: Proxy (c :> b)) (Proxy :: Proxy (If (Equal x a) b a))
  {-# INLINE getIndex #-}

data AppendList a where
  Init :: AppendList a
  (:>) :: AppendList a -> a -> AppendList a

type family Equal (a :: Op) (b :: Op) where
  Equal a a = True
  Equal _ _ = False

type family If (c :: Bool) (a :: Op) (b :: Op) where
  If True a _ = a
  If False _ b = b

class BoolSingleton (b :: Bool) where
  getBoolType :: Proxy b -> Bool

instance BoolSingleton False where
  getBoolType _ = False
  {-# INLINE getBoolType #-}

instance BoolSingleton True where
  getBoolType _ = True
  {-# INLINE getBoolType #-}

type family Merge (as :: AppendList Op) (bs :: AppendList Op) where
  Merge as bs = Merge' as bs as

type family Merge' (as :: AppendList Op) (bs :: AppendList Op) (cs :: AppendList Op) where
  Merge' as Init _ = as
  Merge' (_ :> a) (_ :> a) _ = TypeError (Text "ERR: Overlapping capabilities when combining worlds")
  Merge' (as :> _) (bs :> b) cs = Merge' as (bs :> b) cs
  Merge' Init (bs :> b) cs = Merge' cs bs cs :> b 

type family Contains (as :: AppendList Op) (a :: Op) where
  Contains (_ :> a) a = True
  Contains (as :> _) a = Contains as a
  Contains _ _ = False

type family And (a :: Bool) (b :: Bool) where
  And True True = True
  And _ _ = False

class MultiGet' (b :: Bool) (caps :: AppendList Op) a where
  multiGet' :: MonadIO m => Proxy b -> Entity -> WorldT caps m (Maybe a)

type Gettable caps a = Contains caps (Get a)
type MultiGet caps a = MultiGet' (Gettable caps a) caps a

instance HasCapability caps (Get a) => MultiGet' True caps a where
  multiGet' _ e = eGet e

instance (MultiGet caps a, MultiGet caps b) => MultiGet' False caps (a,b) where
  multiGet' _ e = liftA2 (liftA2 (,))
    (multiGet' (Proxy @(Gettable caps a)) e) 
    (multiGet' (Proxy @(Gettable caps b)) e)

instance (MultiGet caps a, MultiGet caps b, MultiGet caps c) => MultiGet' False caps (a,b,c) where
  multiGet' _ e = liftA3 (liftA3 (,,))
    (multiGet' (Proxy @(Gettable caps a)) e) 
    (multiGet' (Proxy @(Gettable caps b)) e)
    (multiGet' (Proxy @(Gettable caps c)) e)

instance (MultiGet caps a, MultiGet caps b, MultiGet caps c, MultiGet caps d) => MultiGet' False caps (a,b,c,d) where
  multiGet' _ e = liftA4 (liftA4 (,,,))
    (multiGet' (Proxy @(Gettable caps a)) e) 
    (multiGet' (Proxy @(Gettable caps b)) e)
    (multiGet' (Proxy @(Gettable caps c)) e)
    (multiGet' (Proxy @(Gettable caps d)) e)
    where liftA4 f a b c d = f <$> a <*> b <*> c <*> d 

instance (MultiGet caps a, MultiGet caps b, MultiGet caps c, MultiGet caps d, MultiGet caps e) => MultiGet' False caps (a,b,c,d,e) where
  multiGet' _ e = liftA5 (liftA5 (,,,,))
    (multiGet' (Proxy @(Gettable caps a)) e) 
    (multiGet' (Proxy @(Gettable caps b)) e)
    (multiGet' (Proxy @(Gettable caps c)) e)
    (multiGet' (Proxy @(Gettable caps d)) e)
    (multiGet' (Proxy @(Gettable caps e)) e)
    where liftA5 f a b c d e = f <$> a <*> b <*> c <*> d <*> e

instance (MultiGet caps a, MultiGet caps b, MultiGet caps c, MultiGet caps d, MultiGet caps e, MultiGet caps f) => MultiGet' False caps (a,b,c,d,e,f) where
  multiGet' _ e = liftA6 (liftA6 (,,,,,))
    (multiGet' (Proxy @(Gettable caps a)) e) 
    (multiGet' (Proxy @(Gettable caps b)) e)
    (multiGet' (Proxy @(Gettable caps c)) e)
    (multiGet' (Proxy @(Gettable caps d)) e)
    (multiGet' (Proxy @(Gettable caps e)) e)
    (multiGet' (Proxy @(Gettable caps f)) e)
    where liftA6 f a b c d e f' = f <$> a <*> b <*> c <*> d <*> e <*> f'

class MultiSet' (b :: Bool) (caps :: AppendList Op) a where
  multiSet' :: MonadIO m => Proxy b -> Entity -> a -> WorldT caps m ()

type Settable caps a = Contains caps (Set a)
type MultiSet caps a = MultiSet' (Settable caps a) caps a

instance HasCapability caps (Set a) => MultiSet' True caps a where
  multiSet' _ e a = eSet e a

instance (MultiSet caps a, MultiSet caps b) => MultiSet' False caps (a,b) where
  multiSet' _ e (a,b) = 
    multiSet' (Proxy @(Settable caps a)) e a *>
    multiSet' (Proxy @(Settable caps b)) e b

instance (MultiSet caps a, MultiSet caps b, MultiSet caps c) => MultiSet' False caps (a,b,c) where
  multiSet' _ e (a,b,c) = 
    multiSet' (Proxy @(Settable caps a)) e a *>
    multiSet' (Proxy @(Settable caps b)) e b *>
    multiSet' (Proxy @(Settable caps c)) e c

instance (MultiSet caps a, MultiSet caps b, MultiSet caps c, MultiSet caps d) => MultiSet' False caps (a,b,c,d) where
  multiSet' _ e (a,b,c,d) = 
    multiSet' (Proxy @(Settable caps a)) e a *>
    multiSet' (Proxy @(Settable caps b)) e b *>
    multiSet' (Proxy @(Settable caps c)) e c *>
    multiSet' (Proxy @(Settable caps d)) e d

instance (MultiSet caps a, MultiSet caps b, MultiSet caps c, MultiSet caps d, MultiSet caps e) => MultiSet' False caps (a,b,c,d,e) where
  multiSet' _ ent (a,b,c,d,e) = 
    multiSet' (Proxy @(Settable caps a)) ent a *>
    multiSet' (Proxy @(Settable caps b)) ent b *>
    multiSet' (Proxy @(Settable caps c)) ent c *>
    multiSet' (Proxy @(Settable caps d)) ent d *>
    multiSet' (Proxy @(Settable caps e)) ent e

instance (MultiSet caps a, MultiSet caps b, MultiSet caps c, MultiSet caps d, MultiSet caps e, MultiSet caps f) => MultiSet' False caps (a,b,c,d,e,f) where
  multiSet' _ ent (a,b,c,d,e,f) = 
    multiSet' (Proxy @(Settable caps a)) ent a *>
    multiSet' (Proxy @(Settable caps b)) ent b *>
    multiSet' (Proxy @(Settable caps c)) ent c *>
    multiSet' (Proxy @(Settable caps d)) ent d *>
    multiSet' (Proxy @(Settable caps e)) ent e *>
    multiSet' (Proxy @(Settable caps f)) ent f

class MultiDestroy' (b :: Bool) (caps :: AppendList Op) a where
  multiDestroy' :: MonadIO m => Proxy b -> Entity -> Proxy a -> WorldT caps m ()

type Destroyable caps a = Contains caps (Destroy a)
type MultiDestroy caps a = MultiDestroy' (Destroyable caps a) caps a

instance HasCapability caps (Destroy a) => MultiDestroy' True caps a where
  multiDestroy' _ e p = eDestroy e p

instance (MultiDestroy caps a, MultiDestroy caps b) => MultiDestroy' False caps (a,b) where
  multiDestroy' _ e _ = 
    multiDestroy' (Proxy @(Destroyable caps a)) e (Proxy @a) *>
    multiDestroy' (Proxy @(Destroyable caps b)) e (Proxy @b)

instance (MultiDestroy caps a, MultiDestroy caps b, MultiDestroy caps c) => MultiDestroy' False caps (a,b,c) where
  multiDestroy' _ e _ = 
    multiDestroy' (Proxy @(Destroyable caps a)) e (Proxy @a) *>
    multiDestroy' (Proxy @(Destroyable caps b)) e (Proxy @b) *>
    multiDestroy' (Proxy @(Destroyable caps c)) e (Proxy @c)

instance (MultiDestroy caps a, MultiDestroy caps b, MultiDestroy caps c, MultiDestroy caps d) => MultiDestroy' False caps (a,b,c,d) where
  multiDestroy' _ e _ = 
    multiDestroy' (Proxy @(Destroyable caps a)) e (Proxy @a) *>
    multiDestroy' (Proxy @(Destroyable caps b)) e (Proxy @b) *>
    multiDestroy' (Proxy @(Destroyable caps c)) e (Proxy @c) *>
    multiDestroy' (Proxy @(Destroyable caps d)) e (Proxy @d)

instance (MultiDestroy caps a, MultiDestroy caps b, MultiDestroy caps c, MultiDestroy caps d, MultiDestroy caps e) => MultiDestroy' False caps (a,b,c,d,e) where
  multiDestroy' _ e _ = 
    multiDestroy' (Proxy @(Destroyable caps a)) e (Proxy @a) *>
    multiDestroy' (Proxy @(Destroyable caps b)) e (Proxy @b) *>
    multiDestroy' (Proxy @(Destroyable caps c)) e (Proxy @c) *>
    multiDestroy' (Proxy @(Destroyable caps d)) e (Proxy @d) *>
    multiDestroy' (Proxy @(Destroyable caps e)) e (Proxy @e)

instance (MultiDestroy caps a, MultiDestroy caps b, MultiDestroy caps c, MultiDestroy caps d, MultiDestroy caps e, MultiDestroy caps f) => MultiDestroy' False caps (a,b,c,d,e,f) where
  multiDestroy' _ e _ = 
    multiDestroy' (Proxy @(Destroyable caps a)) e (Proxy @a) *>
    multiDestroy' (Proxy @(Destroyable caps b)) e (Proxy @b) *>
    multiDestroy' (Proxy @(Destroyable caps c)) e (Proxy @c) *>
    multiDestroy' (Proxy @(Destroyable caps d)) e (Proxy @d) *>
    multiDestroy' (Proxy @(Destroyable caps e)) e (Proxy @e) *>
    multiDestroy' (Proxy @(Destroyable caps f)) e (Proxy @f)

class MultiMember' (b :: Bool) (caps :: AppendList Op) a where
  multiMember' :: MonadIO m => Proxy b -> Proxy a -> WorldT caps m (Entities a)

type Memberable caps a = Contains caps (Member a)
type MultiMember caps a = MultiMember' (Memberable caps a) caps a

instance HasCapability caps (Member a) => MultiMember' True caps a where
  multiMember' _ p = eMember p

instance (MultiMember caps a, MultiMember caps b) => MultiMember' False caps (a,b) where
  multiMember' _ _ = liftA2 (intersectEntities (,))
    (multiMember' (Proxy @(Memberable caps a)) (Proxy @a)) 
    (multiMember' (Proxy @(Memberable caps b)) (Proxy @b))

instance (MultiMember caps a, MultiMember caps b, MultiMember caps c) => MultiMember' False caps (a,b,c) where
  multiMember' _ _ = liftA3 intersect3
    (multiMember' (Proxy @(Memberable caps a)) (Proxy @a)) 
    (multiMember' (Proxy @(Memberable caps b)) (Proxy @b))
    (multiMember' (Proxy @(Memberable caps c)) (Proxy @c))
    where intersect3 a b c = intersectEntities (\a (b,c) -> (a,b,c)) a $ intersectEntities (,) b c

instance (MultiMember caps a, MultiMember caps b, MultiMember caps c, MultiMember caps d) => MultiMember' False caps (a,b,c,d) where
  multiMember' _ _ = liftA4 intersect4
    (multiMember' (Proxy @(Memberable caps a)) (Proxy @a)) 
    (multiMember' (Proxy @(Memberable caps b)) (Proxy @b))
    (multiMember' (Proxy @(Memberable caps c)) (Proxy @c))
    (multiMember' (Proxy @(Memberable caps d)) (Proxy @d))
    where 
      liftA4 f a b c d = f <$> a <*> b <*> c <*> d 
      intersect4 a b c d = intersectEntities (\(a,b) (c,d) -> (a,b,c,d)) (intersectEntities (,) a b) (intersectEntities (,) c d)

instance (MultiMember caps a, MultiMember caps b, MultiMember caps c, MultiMember caps d, MultiMember caps e) => MultiMember' False caps (a,b,c,d,e) where
  multiMember' _ _ = liftA5 intersect5
    (multiMember' (Proxy @(Memberable caps a)) (Proxy @a)) 
    (multiMember' (Proxy @(Memberable caps b)) (Proxy @b))
    (multiMember' (Proxy @(Memberable caps c)) (Proxy @c))
    (multiMember' (Proxy @(Memberable caps d)) (Proxy @d))
    (multiMember' (Proxy @(Memberable caps e)) (Proxy @e))
    where 
      liftA5 f a b c d e = f <$> a <*> b <*> c <*> d <*> e
      intersect5 a b c d e = intersectEntities (\a (b,c,d,e) -> (a,b,c,d,e)) a (intersectEntities (\(a,b) (c,d) -> (a,b,c,d)) (intersectEntities (,) b c) (intersectEntities (,) d e))

instance (MultiMember caps a, MultiMember caps b, MultiMember caps c, MultiMember caps d, MultiMember caps e, MultiMember caps f) => MultiMember' False caps (a,b,c,d,e,f) where
  multiMember' _ _ = liftA6 intersect6
    (multiMember' (Proxy @(Memberable caps a)) (Proxy @a)) 
    (multiMember' (Proxy @(Memberable caps b)) (Proxy @b))
    (multiMember' (Proxy @(Memberable caps c)) (Proxy @c))
    (multiMember' (Proxy @(Memberable caps d)) (Proxy @d))
    (multiMember' (Proxy @(Memberable caps e)) (Proxy @e))
    (multiMember' (Proxy @(Memberable caps f)) (Proxy @f))
    where 
      liftA6 f a b c d e f' = f <$> a <*> b <*> c <*> d <*> e <*> f'
      intersect6 a b c d e f = intersectEntities (\(a,b,c,d) (e,f) -> (a,b,c,d,e,f)) (intersectEntities (\(a,b) (c,d) -> (a,b,c,d)) (intersectEntities (,) a b) (intersectEntities (,) c d)) (intersectEntities (,) e f)
