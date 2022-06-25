{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Data.SimplifiedFormula.Agents.Out where

import Control.Monad
import qualified Data.HashMap.Strict as M
import Data.Hashable
import Data.IORef
import {-# SOURCE #-} qualified Data.SimplifiedFormula.Agents.And as AndAgent
import Data.SimplifiedFormula.Utils.Fold
import qualified Data.SimplifiedFormula.Utils.IdMap as IdMap

data Node
  = And AndAgent.Self
  | Leaf (IORef (Maybe Message))

newtype Env = Env (IORef Integer)

newEnv :: IO Env
newEnv = Env <$> newIORef 0

new :: Triggerer -> Node -> Env -> IO Self
new triggerer implementation (Env env) = do
  outId <- readIORef env
  writeIORef env (outId + 1)
  return Self{..}

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

newTriggerer :: IO Triggerer
newTriggerer = do
  listeners <- newIORef IdMap.empty
  pendings <- newIORef M.empty
  return Triggerer{..}

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
          writeIORef pendings (M.delete key pendings_)
          listeners_ <- readIORef listeners
          IdMap.get key listeners_ val
          rec
        Nothing -> return ()

addListener :: (IdMap.Key Self -> Listener) -> Triggerer -> IO (IdMap.Key Self)
addListener listener Triggerer{listeners} = do
  listeners_ <- readIORef listeners
  let (key, listeners_') = IdMap.add (listener $ IdMap.next listeners_') listeners_
  writeIORef listeners listeners_'
  return key

parentCount :: Triggerer -> IO Int
parentCount Triggerer{..} = do
  listeners_ <- readIORef listeners
  return $ foldToLimitedSize IdMap.foldElems 2 listeners_

removeListener :: IdMap.Key Self -> Self -> IO ()
removeListener key Self{triggerer = Triggerer{..}, implementation} = do
  listeners_ <- readIORef listeners
  let listeners_' = IdMap.remove key listeners_
  writeIORef listeners listeners_'
  let parentCount = foldToLimitedSize IdMap.foldElems 2 listeners_'
  case implementation of
    And andAgent -> AndAgent.onDecRef andAgent parentCount
    Leaf _ -> return ()

state :: Self -> IO (Maybe Message)
state Self{implementation = And andAgent} = AndAgent.state andAgent
state Self{implementation = Leaf msg} = readIORef msg
