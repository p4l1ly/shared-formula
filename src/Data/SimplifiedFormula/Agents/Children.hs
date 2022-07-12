{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Data.SimplifiedFormula.Agents.Children where

import Control.Monad
import Data.Foldable
import Data.Function.Apply
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.Hashable
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

oldState :: Message -> State
oldState (Add old _ _) = old
oldState (Remove old _ _) = old
oldState (Replace old _ _ _) = old

type Listener = Message -> IO ()
type Listeners = IORef (IdMap.IdMap Self (IdMap.Key Self -> Listener))
type Pendings = IORef (M.HashMap (IdMap.Key Self) Message)
data Self = Self
  { listeners :: Listeners
  , pendings :: Pendings
  , childs :: IORef (M.HashMap Out.Self (IdMap.Key Out.Self))
  }

data AddChildResult = Added Out.Self | Present | Eval Bool

keySet :: Hashable k => M.HashMap k v -> S.HashSet k
keySet = S.fromList . M.keys

new :: IO Self
new = do
  listeners <- newIORef IdMap.empty
  childs <- newIORef M.empty
  pendings <- newIORef M.empty
  return Self{..}

addChild :: Self -> Out.Env -> Out.Self -> IO AddChildResult
addChild Self{..} outEnv = rec
  where
    rec :: Out.Self -> IO AddChildResult
    rec child = do
      Out.state child outEnv >>= \case
        Just msg -> case msg of
          Out.Eval b -> return (Eval b)
          Out.Redirect child' -> do
            rec child'
        Nothing -> do
          childs_ <- readIORef childs
          if child `M.member` childs_
            then return Present
            else return $ Added child

confirmAddChild :: Self -> Out.Env -> Listener -> Out.Self -> IO ()
confirmAddChild self@Self{..} outEnv listener child = do
  childs_ <- readIORef childs
  key <- flip Out.addListener (Out.triggerer child) \key msg -> do
    childs_ <- readIORef childs
    let !childs_' = M.delete child childs_
        childSet = keySet childs_
        childSet' = keySet childs_'
    writeIORef childs childs_'
    case msg of
      Out.Eval _ ->
        triggerListeners listener listeners pendings $
          Remove childSet childSet' (S.singleton child)
      Out.Redirect child' -> do
        addChild self outEnv child' >>= \case
          Added child'' -> do
            confirmAddChild self outEnv listener child''
            childs_'' <- readIORef childs
            triggerListeners listener listeners pendings $
              Replace
                childSet
                (keySet childs_'')
                (S.singleton child)
                (S.singleton child'')
          Present ->
            triggerListeners listener listeners pendings $
              Remove childSet childSet' (S.singleton child)
          Eval _ ->
            triggerListeners listener listeners pendings $
              Remove childSet childSet' (S.singleton child)
    Out.removeListener key child outEnv
  writeIORef childs (M.insert child key childs_)

state :: Self -> IO State
state Self{childs} = keySet <$> readIORef childs

addListener :: (IdMap.Key Self -> Listener) -> Self -> IO (IdMap.Key Self)
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
  modifyIORef' pendings (M.delete key)

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
          IdMap.get key listeners_ key val
          rec
        Nothing -> return ()

free :: Self -> Out.Env -> IO ()
free Self{..} outEnv = do
  childs_ <- readIORef childs
  M.foldlWithKey -$ pure () -$ childs_ $ \rest child key ->
    rest <* Out.removeListener key child outEnv
