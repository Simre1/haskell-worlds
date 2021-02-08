# Haskell Worlds

Haskell Worlds is an *Entity Component System* (ECS) implemented in Haskell. Design choices have been heavily influenced by *apecs*, but I have tried to create an even more ergonomic interface. The most interesting feature is that you can immediately add new components without any boilerplate. It is also possible to dynamically add more components to a world as the application runs.

Here's an example:
```haskell
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
```

## Getting started

*HWorlds* does not require any special build instructions, but you need to use *ghc-8.10*. The following should then work without any issues:
```bash
git clone https://github.com/Simre1/haskell-worlds
cd haskell-worlds
cabal build
```
You can now run the example app or a minimal benchmark which compares *apecs* and *haskell-worlds*:
```haskell 
cabal run app
cabal run bench
```
