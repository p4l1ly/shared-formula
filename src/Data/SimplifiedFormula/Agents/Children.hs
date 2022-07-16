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
  = Init State
  | Remove State State (S.HashSet Out.Self)
  | Add State State (S.HashSet Out.Self)
  | Replace State State (S.HashSet Out.Self) (S.HashSet Out.Self)
  deriving (Show, Eq)

instance Semigroup Message where
  Add old1 _ diff1 <> Add _ new2 diff2 = Add old1 new2 (diff1 `S.union` diff2)
  Remove old1 _ diff1 <> Remove _ new2 diff2 = Remove old1 new2 (diff1 `S.union` diff2)
  _ <> _ = error "Children.Message.<> unimplemented: nontrivial TODO"

newState :: Message -> State
newState (Init new) = new
newState (Add _ new _) = new
newState (Remove _ new _) = new
newState (Replace _ new _ _) = new

oldState :: Message -> State
oldState (Init old) = old
oldState (Add old _ _) = old
oldState (Remove old _ _) = old
oldState (Replace old _ _ _) = old

type InternalListener = Message -> IO ()
type ExternalListener = IdMap.Key Self -> Message -> IO ()
data Self = Self
  { listeners :: IORef (IdMap.IdMap Self ExternalListener)
  , pendings :: IORef (M.HashMap (IdMap.Key Self) Message)
  , pendingRemovals :: IORef (M.HashMap Out.Self (IdMap.Key Out.Self))
  , childs :: IORef (M.HashMap Out.Self (IdMap.Key Out.Self))
  }

data AddChildResult = Added Out.Self Message | Present | Eval Bool

keySet :: Hashable k => M.HashMap k v -> S.HashSet k
keySet = S.fromList . M.keys

new :: IO Self
new = do
  listeners <- newIORef IdMap.empty
  childs <- newIORef M.empty
  pendings <- newIORef M.empty
  pendingRemovals <- newIORef M.empty
  return Self{..}

addChild :: Out.Env -> Out.Self -> Message -> IO AddChildResult
addChild outEnv child msg = rec child
  where
    rec :: Out.Self -> IO AddChildResult
    rec child = do
      Out.state child outEnv >>= \case
        Just msg -> case msg of
          Out.Eval b -> return (Eval b)
          Out.Redirect child' -> do
            rec child'
        Nothing -> return do
          if child `S.member` (newState msg)
            then Present
            else Added child $ case msg of
              Init old -> Add old (S.insert child old) (S.singleton child)
              Add old new plus -> Add old (S.insert child new) (S.insert child plus)
              Remove old new minus
                | S.member child minus ->
                    let minus' = S.delete child minus
                     in if S.null minus'
                          then Init old
                          else Remove old (S.insert child new) minus'
                | otherwise ->
                    Replace old (S.insert child new) minus (S.singleton child)
              Replace old new minus plus
                | S.member child minus ->
                    let minus' = S.delete child minus
                     in if S.null minus'
                          then Add old (S.insert child new) plus
                          else Replace old (S.insert child new) minus' plus
                | otherwise ->
                    Replace old (S.insert child new) minus (S.insert child plus)

onChildMsg ::
  Self ->
  InternalListener ->
  Out.Env ->
  Out.Self ->
  IdMap.Key Out.Self ->
  Out.Message ->
  IO ()
onChildMsg self@Self{..} listener outEnv child key msg = do
  pendingRemovals_ <- readIORef pendingRemovals
  when (pendingRemovals_ M.!? child /= Just key) do
    childs_ <- readIORef childs
    let !childs_' = M.delete child childs_
        childSet = keySet childs_
        childSet' = keySet childs_'
        removal = Remove childSet childSet' (S.singleton child)
    case msg of
      Out.Eval _ -> listener removal
      Out.Redirect child' -> do
        addChild outEnv child' removal >>= \case
          Added child'' msg -> listener msg
          Present -> listener removal
          Eval _ -> listener removal

applyAdd ::
  Self ->
  InternalListener ->
  Out.Env ->
  M.HashMap Out.Self (IdMap.Key Out.Self) ->
  S.HashSet Out.Self ->
  IO (M.HashMap Out.Self (IdMap.Key Out.Self))
applyAdd self@Self{..} listener outEnv childs_ plus = do
  pendingRemovals_ <- readIORef pendingRemovals
  foldM -$ childs_ -$ plus $ \childs_' child -> do
    key <-
      flip Out.addListener (Out.triggerer child) (onChildMsg self listener outEnv child)
    return $ M.insert child key childs_'

applyRem ::
  Self ->
  M.HashMap Out.Self (IdMap.Key Out.Self) ->
  S.HashSet Out.Self ->
  IO (M.HashMap Out.Self (IdMap.Key Out.Self))
applyRem Self{..} childs_ minus = do
  pendingRemovals_ <- readIORef pendingRemovals
  let pendingRemovals_' = foldr -$ pendingRemovals_ -$ minus $
        \child pendingRemovals_' ->
          M.insert child (childs_ M.! child) pendingRemovals_'
  let childs_' = foldr -$ childs_ -$ minus $ \child childs_' ->
        M.delete child childs_'
  writeIORef pendingRemovals pendingRemovals_'
  return childs_'

apply :: Self -> Out.Env -> InternalListener -> Message -> IO ()
apply self@Self{..} outEnv listener msg = do
  case msg of
    Init _ -> return ()
    Remove _ _ minus -> do
      childs_ <- readIORef childs
      childs_' <- applyRem self childs_ minus
      writeIORef childs childs_'
    Add _ _ plus -> do
      childs_ <- readIORef childs
      childs_' <- applyAdd self listener outEnv childs_ plus
      writeIORef childs childs_'
    Replace _ _ minus plus -> do
      childs_ <- readIORef childs
      childs_' <- applyRem self childs_ minus
      childs_'' <- applyAdd self listener outEnv childs_' plus
      writeIORef childs childs_''

triggerListeners :: Self -> Out.Env -> Message -> IO ()
triggerListeners self@Self{..} outEnv msg = do
  pendings_ <- readIORef pendings
  listeners_ <- readIORef listeners
  writeIORef pendings $
    M.mapWithKey
      (\key _ -> maybe msg (<> msg) $ M.lookup key pendings_)
      (IdMap.items listeners_)
  handlePendings self outEnv

state :: Self -> IO State
state Self{childs} = keySet <$> readIORef childs

addListener :: ExternalListener -> Self -> IO (IdMap.Key Self)
addListener newListener Self{..} = do
  listeners_ <- readIORef listeners
  let (key, listeners_') = IdMap.add newListener listeners_
  writeIORef listeners listeners_'
  return key

-- Children's parents are only weak references.
-- We decref all children in `free` - after our master says.
removeListener :: IdMap.Key Self -> Self -> IO ()
removeListener key Self{..} = do
  modifyIORef' listeners (IdMap.remove key)
  modifyIORef' pendings (M.delete key)

handlePendings :: Self -> Out.Env -> IO ()
handlePendings Self{..} outEnv = rec
  where
    rec = do
      pendings_ <- readIORef pendings
      case foldToPeak (M.foldrWithKey' . curry) pendings_ of
        Just (key, val) -> do
          writeIORef pendings (M.delete key pendings_)
          listeners_ <- readIORef listeners
          IdMap.get key listeners_ key val
          rec
        Nothing -> do
          pendingRemovals_ <- readIORef pendingRemovals
          case foldToPeak (M.foldrWithKey' . curry) pendingRemovals_ of
            Just (child, key) -> do
              writeIORef pendingRemovals (M.delete child pendingRemovals_)
              Out.removeListener key child outEnv
              rec
            Nothing -> return ()

free :: Self -> Out.Env -> IO ()
free Self{..} outEnv = do
  childs_ <- readIORef childs
  M.foldlWithKey -$ pure () -$ childs_ $ \rest child key ->
    rest <* Out.removeListener key child outEnv
  pendingRemovals_ <- readIORef pendingRemovals
  M.foldlWithKey -$ pure () -$ pendingRemovals_ $ \rest child key ->
    rest <* Out.removeListener key child outEnv
