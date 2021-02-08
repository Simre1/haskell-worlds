{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}


module Main where

import HWorlds
import Control.Monad (replicateM_)
import Control.Monad.IO.Class (MonadIO(liftIO))

data Position = Position Double Double
data Velocity = Velocity Double Double

main :: IO ()
main = do
  positionWorld <- mapWorld @Position
  velocityWorld <- mapWorld @Velocity
  entityWorld <- entityWorld
  let myWorld = mergeWorld positionWorld $ mergeWorld velocityWorld entityWorld
  runWorldT myWorld $ do
    replicateM_ 20 $ do
      e <- initEntity (Position 0 0)
      e <- initEntity (Position 10 10, Velocity 0 5)
      initEntity (Position 5 5)

    replicateM_ 50 $ do

      cmap $ \(Velocity dx dy) -> Velocity dx (pred dy)
      cmap $ \(Position x y, Velocity dx dy) -> Position (dx + x) (dy + y)
      cmapM_ $ \(Position x y, Velocity _ _) -> liftIO $ print (x,y)
