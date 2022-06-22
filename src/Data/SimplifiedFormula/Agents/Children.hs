{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Data.SimplifiedFormula.Agents.Children where

import Control.Monad
import Data.Foldable
import Data.Function.Apply
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.IORef
import qualified Data.SimplifiedFormula.Agents.Out as Out
import Data.SimplifiedFormula.Utils.Fold
import qualified Data.SimplifiedFormula.Utils.IdMap as IdMap

type State = S.HashSet Out.Self

data Message
  = Add State State (S.HashSet Out.Self)
  | Remove State State (S.HashSet Out.Self)
  | Replace State State (S.HashSet Out.Self) (S.HashSet Out.Self)

instance Semigroup Message where
  Add old1 _ diff1 <> Add _ new2 diff2 = Add old1 new2 (diff1 `S.union` diff2)
  Remove old1 _ diff1 <> Remove _ new2 diff2 = Remove old1 new2 (diff1 `S.union` diff2)
  _ <> _ = error "Children.Message.<> unimplemented: nontrivial TODO"

newState :: Message -> State
newState (Add _ new _) = new
newState (Remove _ new _) = new
newState (Replace _ new _ _) = new

type Listener = Message -> IO ()
type Listeners = IORef (IdMap.IdMap Self Listener)
type Pendings = IORef (M.HashMap (IdMap.Key Self) Message)
data Self = Self
  { listener :: Listener
  , listeners :: Listeners
  , pendings :: Pendings
  , childs :: IORef (S.HashSet Out.Self)
  }

new :: [Out.Self] -> Listener -> IO (Self, (Bool, Bool))
new (S.fromList -> childs_) listener = do
  listeners <- newIORef IdMap.empty
  childs <- newIORef childs_
  pendings <- newIORef M.empty

  let onRemoveChild key child = do
        childs_ <- readIORef childs
        let childs_' = S.delete child childs_
        writeIORef childs childs_'
        triggerListeners listener listeners pendings $
          Remove childs_ childs_' (S.singleton child)
        Out.removeListener key child

  let onRedirectChild key child child' = do
        childs_ <- readIORef childs
        let childs_' = S.insert child' $ S.delete child childs_
        writeIORef childs childs_'
        triggerListeners listener listeners pendings $
          Replace childs_ childs_' (S.singleton child) (S.singleton child')
        Out.removeListener key child

  let addChildListener :: Out.Self -> IO (Maybe Out.Message)
      addChildListener child = do
        Out.state child >>= \case
          Just msg -> case msg of
            Out.Eval _ -> return (Just msg)
            Out.Redirect child' -> addChildListener child'
          Nothing ->
            Nothing <$ flip Out.addListener (Out.triggerer child) \key -> \case
              Out.Eval _ -> onRemoveChild key child
              Out.Redirect child' -> do
                addChildListener child' >>= \case
                  Nothing -> onRedirectChild key child child'
                  Just msg -> case msg of
                    Out.Eval _ -> onRemoveChild key child
                    Out.Redirect child' -> onRedirectChild key child child'

  hasBools <- foldM -$ (False, False) -$ childs_ $
    \hasBools@(hasFalse, hasTrue) child -> do
      addChildListener child >>= \case
        Nothing -> return hasBools
        Just (Out.Eval False) -> do
          modifyIORef childs (S.delete child)
          return (True, hasTrue)
        Just (Out.Eval True) -> do
          modifyIORef childs (S.delete child)
          return (hasFalse, True)
        Just (Out.Redirect child') -> do
          modifyIORef childs (S.insert child' . S.delete child)
          return hasBools

  return (Self{..}, hasBools)

state :: Self -> IO State
state Self{childs} = readIORef childs

addListener :: Listener -> Self -> IO (IdMap.Key Self)
addListener newListener Self{..} = do
  listeners_ <- readIORef listeners
  let (key, listeners_') = IdMap.add newListener listeners_
  writeIORef listeners listeners_'
  return key

-- Children do not listen to anything => we really just remove the listener TODO no
removeListener :: (IdMap.Key Self) -> Self -> IO ()
removeListener key Self{..} = modifyIORef' listeners (IdMap.remove key)

triggerListeners :: Listener -> Listeners -> Pendings -> Message -> IO ()
triggerListeners listener listeners pendings msg = do
  pendings_ <- readIORef pendings
  listeners_ <- readIORef listeners
  writeIORef pendings $
    M.mapWithKey
      (\key _ -> maybe msg (<> msg) $ M.lookup key pendings_)
      (IdMap.items listeners_)
  listener msg
  handlePendings listeners pendings

handlePendings :: Listeners -> Pendings -> IO ()
handlePendings listeners pendings = rec
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
