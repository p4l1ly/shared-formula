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

type Listener = Message -> IO ()
type Listeners = IORef (IdMap.IdMap Self Listener)
type Pendings = IORef (M.HashMap (IdMap.Key Self) Message)
data Self = Self
  { listener :: Listener
  , listeners :: Listeners
  , pendings :: Pendings
  , childs :: IORef (M.HashMap Out.Self (IdMap.Key Out.Self))
  }

data AddChildResult = Added Out.Self | Present | Eval Bool

keySet :: Hashable k => M.HashMap k v -> S.HashSet k
keySet = S.fromList . M.keys

new :: [Out.Self] -> Listener -> IO (Self, [AddChildResult])
new childs_ listener = do
  listeners <- newIORef IdMap.empty
  childs <- newIORef M.empty
  pendings <- newIORef M.empty

  let addChild :: Out.Self -> IO AddChildResult
      addChild child = do
        Out.state child >>= \case
          Just msg -> case msg of
            Out.Eval b -> return (Eval b)
            Out.Redirect child' -> addChild child'
          Nothing -> do
            childs_ <- readIORef childs
            if child `M.member` childs_
              then return Present
              else do
                key <- flip Out.addListener (Out.triggerer child) \key msg -> do
                  childs_ <- readIORef childs
                  let childs_' = M.delete child childs_
                      childSet = keySet childs_
                      childSet' = keySet childs_'
                  writeIORef childs childs_'
                  case msg of
                    Out.Eval _ ->
                      triggerListeners listener listeners pendings $
                        Remove childSet childSet' (S.singleton child)
                    Out.Redirect child' -> do
                      addChild child' >>= \case
                        Added child'' -> do
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
                  Out.removeListener key child
                writeIORef childs (M.insert child key childs_)
                return $ Added child

  (Self{..},) <$> mapM addChild childs_

state :: Self -> IO State
state Self{childs} = keySet <$> readIORef childs

addListener :: Listener -> Self -> IO (IdMap.Key Self)
addListener newListener Self{..} = do
  listeners_ <- readIORef listeners
  let (key, listeners_') = IdMap.add newListener listeners_
  writeIORef listeners listeners_'
  return key

-- Children's parents are only weak references.
-- We decref all children in `free` - after our master (Out) says.
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

free :: Self -> IO ()
free Self{..} = do
  childs_ <- readIORef childs
  M.foldlWithKey -$ pure () -$ childs_ $ \rest child key ->
    rest <* Out.removeListener key child
