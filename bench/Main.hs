{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}


module Main where

import Apecs as A
import HWorlds.Core as E
import HWorlds.Query as E
import HWorlds.Store.HashMap as E
import HWorlds.Store.Map as E
import Gauge.Main
import Control.Monad

-- I am using Apecs as a performance measurement. Speed seems good enough.

newtype Position = Position (Int, Int)

makeWorldAndComponents "ApecsWorld" [''Position, ''String]

main :: IO ()
main = do
  pure ()

  apecs <- makeApecsTest
  ecsMap <- makeECSTest E.mapWorld
  ecsHash <- makeECSTest E.hashWorld

  defaultMain 
    [
      bgroup "ecs-bench" 
        [ bench "apecs"  $ whnfIO apecs
        , bench "ecsMap" $ whnfIO ecsMap
        , bench "ecsHash" $ whnfIO ecsHash
        ]
    ]

makeApecsTest :: IO (IO ())
makeApecsTest = do
  w <- initApecsWorld
  pure $ flip runSystem w $ do 
    replicateM_ 50 $ do
      A.newEntity (Position (0,0), "Hello")
      A.newEntity (Position (0,1))
    replicateM_ 50 $ do
      A.cmap (\(Position (x,y)) -> Position (x+1, y+1))
      A.cmap (\(Position (x,y), str :: String) -> show (x,y))
      -- A.cmap (\(str :: String, Position (x,y)) -> (Not :: Not Position))
      A.cmap (\(str :: String) -> Position (0,0))

makeECSTest :: (forall a. IO (E.World (E.Init :> E.Get a :> E.Set a :> E.Destroy a :> E.Member a))) -> IO (IO ())
makeECSTest makeWorld = do
  positionWorld <- makeWorld @Position
  stringWorld <- makeWorld @String
  ew <- E.entityWorld
  let w = E.mergeWorld ew $ E.mergeWorld stringWorld positionWorld
  pure $ E.runWorldT w $ do
    replicateM_ 50 $ do
      e1 <- E.initEntity (Position (0,0), "Hello")
      e2 <- E.initEntity (Position (0,1))
      pure ()

    replicateM_ 50 $ do

      E.cmap (\(Position (x,y)) -> Position (x+1, y+1))
      E.cmap (\(Position (x,y), str :: String) -> show (x,y))
      -- E.cmapM_ (\(str :: String, Position (x,y)) -> E.qDestroy (Proxy @Position))
      E.cmap (\(str :: String) -> Position (0,0))