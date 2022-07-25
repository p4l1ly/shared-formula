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

data Message = Message
  { oldState :: State
  , newState :: State
  , negdiff :: S.HashSet Out.Self
  , posdiff :: S.HashSet Out.Self
  }
  deriving (Show, Eq)

instance Semigroup Message where
  Message old1 new1 minus1 plus1 <> Message old2 new2 minus2 plus2 =
    Message old1 new2 (S.union minus1' minus2') (S.union plus1' plus2')
    where
      minus1' = S.difference minus1 plus2
      minus2' = S.difference minus2 plus1
      plus1' = S.difference plus1 minus2
      plus2' = S.difference plus2 minus1

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
addChild outEnv child msg@(Message old new minus plus) = rec child
  where
    rec :: Out.Self -> IO AddChildResult
    rec child = do
      Out.state child outEnv >>= \case
        Just msg -> case msg of
          Out.Eval b -> return (Eval b)
          Out.Redirect child' -> do
            rec child'
        Nothing -> return case () of
          _
            | child `S.member` (newState msg) -> Present
            | S.member child minus ->
                Added child $
                  Message old (S.insert child new) (S.delete child minus) plus
            | otherwise ->
                Added child $
                  Message old (S.insert child new) minus (S.insert child plus)

remChild :: Out.Self -> Message -> Message
remChild child = \case
  Message old new minus plus
    | S.member child plus ->
        let plus' = S.delete child plus
         in Message old (S.delete child new) minus plus'
    | otherwise ->
        Message old (S.delete child new) (S.insert child minus) plus

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
        removal = Message childSet childSet' (S.singleton child) S.empty
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
      flip Out.addListener child (onChildMsg self listener outEnv child)
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
apply self@Self{..} outEnv listener (Message _ _ minus plus) = do
  childs_ <- readIORef childs
  childs_' <- if S.null minus then return childs_ else applyRem self childs_ minus
  childs_'' <-
    if S.null plus
      then return childs_'
      else applyAdd self listener outEnv childs_' plus
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
