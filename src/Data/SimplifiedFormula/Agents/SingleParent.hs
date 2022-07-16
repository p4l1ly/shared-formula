{-# LANGUAGE RecordWildCards #-}

module Data.SimplifiedFormula.Agents.SingleParent where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.IORef
import qualified Data.SimplifiedFormula.Agents.Out as Out
import Data.SimplifiedFormula.Utils.Fold
import qualified Data.SimplifiedFormula.Utils.IdMap as IdMap

data Self = Self
  { listeners :: Listeners
  , pendings :: Pendings
  }

type Listener = IdMap.Key Self -> IO ()
type Listeners = IORef (IdMap.IdMap Self Listener)
type Pendings = IORef (S.HashSet (IdMap.Key Self))

new :: IO Self
new = Self <$> newIORef IdMap.empty <*> newIORef S.empty

state :: Out.Triggerer -> IO Int
state out = Out.parentCount out

addListener :: Listener -> Self -> IO (IdMap.Key Self)
addListener newListener Self{..} = do
  listeners_ <- readIORef listeners
  let (key, listeners_') = IdMap.add newListener listeners_
  writeIORef listeners listeners_'
  return key

-- Children's parents are only weak references.
-- We decref all children in `free` - after our master (Out) says.
removeListener :: IdMap.Key Self -> Self -> IO ()
removeListener key Self{..} = do
  modifyIORef' listeners (IdMap.remove key)
  modifyIORef' pendings (S.delete key)

triggerListeners :: Self -> IO ()
triggerListeners Self{..} = do
  pendings_ <- readIORef pendings
  listeners_ <- readIORef listeners
  writeIORef pendings $ S.fromList $ M.keys $ IdMap.items listeners_
  handlePendings listeners pendings

handlePendings :: Listeners -> Pendings -> IO ()
handlePendings listeners pendings = rec
  where
    rec = do
      pendings_ <- readIORef pendings
      case foldToPeak S.foldr pendings_ of
        Just key -> do
          writeIORef pendings (S.delete key pendings_)
          listeners_ <- readIORef listeners
          IdMap.get key listeners_ key
          rec
        Nothing -> return ()
