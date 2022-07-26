{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Data.SimplifiedFormula.Agents.Out where

import Control.Monad
import Data.Bifunctor
import Data.Functor
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.Hashable
import Data.IORef
import {-# SOURCE #-} qualified Data.SimplifiedFormula.Agents.And as AndAgent
import {-# SOURCE #-} qualified Data.SimplifiedFormula.Agents.ShareSet as ShareSet
import Data.SimplifiedFormula.Utils.Fold
import qualified Data.SimplifiedFormula.Utils.IdMap as IdMap

data Node
  = And AndAgent.Self
  | Leaf
  | ConstLeaf Message

newEnv :: IO Env
newEnv = do
  andShareEnv <- ShareSet.newEnv
  nextId <- newIORef 0
  return Env{..}

new :: Node -> Env -> IO Self
new impl Env{nextId} = do
  listeners <- newIORef IdMap.empty
  pendings <- newIORef Nothing
  !outId <- readIORef nextId
  writeIORef nextId (outId + 1)
  return Self{..}

data Self = Self
  { impl :: Node
  , outId :: Integer
  , listeners :: Listeners
  , pendings :: Pendings
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

newLeaf :: Env -> IO Self
newLeaf env = do
  new Leaf env

newAnd :: [Self] -> Env -> IO (Either Message Self)
newAnd childs env = do
  impl <- AndAgent.new
  self <- new (And impl) env
  AndAgent.init childs env self impl
    <&> maybe (Right self) Left

newAnd' :: [Self] -> Env -> IO (Either Message (Self, AndAgent.Self))
newAnd' childs env = do
  impl <- AndAgent.new
  self <- new (And impl) env
  AndAgent.init childs env self impl
    <&> maybe (Right (self, impl)) Left

type Listener = Message -> IO ()
type Listeners = IORef (IdMap.IdMap Self (IdMap.Key Self -> Listener))
type Pendings = IORef (Maybe (Message, S.HashSet (IdMap.Key Self)))

triggerListeners :: Message -> Self -> IO ()
triggerListeners msg self@Self{..} = do
  readIORef pendings >>= \case
    Nothing -> do
      listeners_ <- readIORef listeners
      writeIORef pendings $ Just (msg, S.fromList $ M.keys $ IdMap.items listeners_)
      handlePendings self
    _ -> return ()

handlePendings :: Self -> IO ()
handlePendings Self{..} = rec
  where
    rec = do
      Just (msg, pendings_) <- readIORef pendings
      case foldToPeak S.foldr pendings_ of
        Just key -> do
          writeIORef pendings (Just (msg, S.delete key pendings_))
          listeners_ <- readIORef listeners
          IdMap.get key listeners_ key msg
          rec
        Nothing -> return ()

addListener :: (IdMap.Key Self -> Listener) -> Self -> IO (IdMap.Key Self)
addListener listener self@Self{..} = do
  listeners_ <- readIORef listeners
  let (key, listeners_') = IdMap.add listener listeners_
  writeIORef listeners listeners_'
  return key

parentCount :: Self -> IO Int
parentCount Self{..} = do
  foldToLimitedSize IdMap.foldElems 2 <$> readIORef listeners

parentCountFull :: Self -> IO Int
parentCountFull Self{..} = readIORef listeners <&> M.size . IdMap.items

removeListener :: IdMap.Key Self -> Self -> Env -> IO ()
removeListener key self@Self{..} env = do
  listeners_ <- readIORef listeners
  let listeners_' = IdMap.remove key listeners_
  writeIORef listeners listeners_'
  modifyIORef' pendings (second (S.delete key) <$>)
  let parentCount = foldToLimitedSize IdMap.foldElems 2 listeners_'
  case impl of
    And andAgent -> AndAgent.onDecRef andAgent parentCount env
    Leaf -> return ()

freeIfZeroParents :: Env -> Self -> IO ()
freeIfZeroParents env self@Self{..} = do
  foldToLimitedSize IdMap.foldElems 1 <$> readIORef listeners >>= \case
    0 -> case impl of
      And andAgent -> AndAgent.onDecRef andAgent 0 env
      Leaf -> return ()
    _ -> return ()

data Env = Env
  { andShareEnv :: ShareSet.EnvRef
  , nextId :: IORef Integer
  }

state :: Self -> IO (Maybe Message)
state self@Self{..} = fmap fst <$> readIORef pendings
