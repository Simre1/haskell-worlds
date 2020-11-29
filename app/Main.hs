{-# LANGUAGE TypeApplications #-}
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
    initEntity (Position 0 0)
    initEntity (Position 10 10, Velocity 0 5)

    replicateM_ 10 $ do

      cmap $ \(Velocity dx dy) -> Velocity dx (pred dy)
      cmap $ \(Position x y, Velocity dx dy) -> Position (dx + x) (dy + y)
      cmapM_ $ \(Position x y) -> liftIO $ print (x,y)
