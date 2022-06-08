{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Data.SimplifiedFormula.Agents.Out where

import qualified Data.HashMap.Strict as M
import Data.Hashable
import Data.IORef
import {-# SOURCE #-} qualified Data.SimplifiedFormula.Agents.And as AndAgent
import Data.SimplifiedFormula.Utils.Fold
import qualified Data.SimplifiedFormula.Utils.IdMap as IdMap

newtype Node
  = And AndAgent.Self

newtype Env = Env (IORef Integer)

data Self = Self
  { implementation :: Node
  , outId :: Integer
  , triggerer :: Triggerer
  }

instance Eq Self where
  a == b = outId a == outId b

instance Hashable Self where
  hashWithSalt = hashUsing outId
  hash = hash . outId

data Message
  = Eval Bool
  | Redirect Self

instance Semigroup Message where
  Eval b <> Redirect _ = Eval b
  Redirect _ <> Eval b = Eval b
  Eval True <> Eval True = Eval True
  Eval False <> Eval False = Eval False
  Eval _ <> Eval _ = error "And.Eval conflict"
  Redirect x <> Redirect y = Redirect x

data Triggerer = Triggerer
  { listeners :: Listeners
  , pendings :: Pendings
  }

type Listener = Message -> IO ()
type Listeners = IORef (IdMap.IdMap Self Listener)
type Pendings = IORef (M.HashMap (IdMap.Key Self) Message)

triggerListeners :: Message -> Triggerer -> IO ()
triggerListeners msg triggerer@Triggerer{..} = do
  pendings_ <- readIORef pendings
  listeners_ <- readIORef listeners
  writeIORef pendings $
    M.mapWithKey
      (\key _ -> maybe msg (<> msg) $ M.lookup key pendings_)
      (IdMap.items listeners_)
  handlePendings triggerer

handlePendings :: Triggerer -> IO ()
handlePendings Triggerer{..} = rec
  where
    rec = do
      pendings_ <- readIORef pendings
      case foldToPeak (M.foldrWithKey' . curry) pendings_ of
        Just (key, val) -> do
          listeners_ <- readIORef listeners
          IdMap.get key listeners_ val
          rec
        Nothing -> return ()

addListener :: (IdMap.Key Self -> Listener) -> Triggerer -> IO ()
addListener listener Triggerer{listeners} = do
  listeners_ <- readIORef listeners
  let (key, listeners_') = IdMap.add (listener $ IdMap.next listeners_') listeners_
  writeIORef listeners listeners_'

removeListener :: IdMap.Key Self -> Self -> IO ()
removeListener key Self{triggerer = Triggerer{..}} = do
  -- TODO notify and deallocate implementation: singleParent
  modifyIORef' listeners (IdMap.remove key)

state :: Self -> IO (Maybe Message)
state Self{implementation = And andAgent} = AndAgent.state andAgent
