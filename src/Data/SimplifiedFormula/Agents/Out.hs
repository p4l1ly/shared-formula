{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Data.SimplifiedFormula.Agents.Out where

import Control.Monad
import Data.Functor
import qualified Data.HashMap.Strict as M
import Data.Hashable
import Data.IORef
import {-# SOURCE #-} qualified Data.SimplifiedFormula.Agents.And as AndAgent
import {-# SOURCE #-} qualified Data.SimplifiedFormula.Agents.ShareSet as ShareSet
import Data.SimplifiedFormula.Utils.Fold
import qualified Data.SimplifiedFormula.Utils.IdMap as IdMap

data Node
  = And AndAgent.Self
  | Leaf (IORef (Maybe Message))
  | ConstLeaf Message

newEnv :: IO Env
newEnv = do
  andShareEnv <- ShareSet.newEnv
  nextId <- newIORef 0
  return Env{..}

new :: Triggerer -> Node -> Env -> IO Self
new triggerer implementation Env{nextId} = do
  !outId <- readIORef nextId
  writeIORef nextId (outId + 1)
  return Self{..}

data Self = Self
  { implementation :: Node
  , outId :: Integer
  , triggerer :: Triggerer
  }

instance Show Self where
  show Self{..} = '^' : show outId

instance Eq Self where
  a == b = outId a == outId b

instance Hashable Self where
  hashWithSalt = hashUsing outId
  hash = hash . outId

data Message
  = Eval Bool
  | Redirect Self
  deriving (Show)

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
type Listeners = IORef (IdMap.IdMap Self (IdMap.Key Self -> Listener))
type Pendings = IORef (M.HashMap (IdMap.Key Self) Message)

triggerListeners :: Message -> Triggerer -> IO ()
triggerListeners msg triggerer@Triggerer{..} = do
  pendings_ <- readIORef pendings
  listeners_ <- readIORef listeners
  let pendings_' =
        M.mapWithKey
          (\key _ -> maybe msg (<> msg) $ M.lookup key pendings_)
          (IdMap.items listeners_)
  writeIORef pendings pendings_'
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
          IdMap.get key listeners_ key val
          rec
        Nothing -> return ()

addListener :: (IdMap.Key Self -> Listener) -> Self -> IO (IdMap.Key Self)
addListener listener self@Self{triggerer = Triggerer{..}} = do
  listeners_ <- readIORef listeners
  let (key, listeners_') = IdMap.add listener listeners_
  writeIORef listeners listeners_'
  return key

parentCount :: Triggerer -> IO Int
parentCount Triggerer{..} = do
  foldToLimitedSize IdMap.foldElems 2 <$> readIORef listeners

parentCountFull :: Triggerer -> IO Int
parentCountFull Triggerer{..} = readIORef listeners <&> M.size . IdMap.items

removeListener :: IdMap.Key Self -> Self -> Env -> IO ()
removeListener key self@Self{triggerer = Triggerer{..}, implementation} env = do
  listeners_ <- readIORef listeners
  let listeners_' = IdMap.remove key listeners_
  writeIORef listeners listeners_'
  modifyIORef' pendings (M.delete key)
  let parentCount = foldToLimitedSize IdMap.foldElems 2 listeners_'
  case implementation of
    And andAgent -> AndAgent.onDecRef andAgent parentCount env
    Leaf _ -> return ()

pingRefCount :: Self -> Env -> IO ()
pingRefCount self@Self{triggerer = Triggerer{..}, implementation} env = do
  listeners_ <- readIORef listeners
  let parentCount = foldToLimitedSize IdMap.foldElems 2 listeners_
  case implementation of
    And andAgent -> AndAgent.onDecRef andAgent parentCount env
    Leaf _ -> return ()

data Env = Env
  { andShareEnv :: ShareSet.EnvRef
  , nextId :: IORef Integer
  }

state :: Self -> Env -> IO (Maybe Message)
state self@Self{implementation = And andAgent} env =
  AndAgent.state andAgent env <&> \case
    Just (Redirect out) | out == self -> Nothing
    msg -> msg
state Self{implementation = Leaf msg} _ = readIORef msg
