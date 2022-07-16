{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Data.SimplifiedFormula.Agents.Swallow where

import Control.Monad
import Data.Foldable
import Data.Function.Apply
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.IORef
import {-# SOURCE #-} qualified Data.SimplifiedFormula.Agents.And as And
import qualified Data.SimplifiedFormula.Agents.Children as Children
import qualified Data.SimplifiedFormula.Agents.Out as Out
import qualified Data.SimplifiedFormula.Agents.SingleParent as SingleParent
import qualified Data.SimplifiedFormula.Utils.IdMap as IdMap

data Message = Replace Out.Self (S.HashSet Out.Self)

type Listener = Message -> IO ()

newtype Self = Self
  { childs :: IORef (M.HashMap Out.Self (IdMap.Key SingleParent.Self))
  }

new :: IO Self
new = Self <$> newIORef M.empty

childsToRemove ::
  M.HashMap Out.Self (IdMap.Key SingleParent.Self) ->
  S.HashSet Out.Self ->
  ( [(Out.Self, IdMap.Key SingleParent.Self)]
  , M.HashMap Out.Self (IdMap.Key SingleParent.Self)
  )
childsToRemove childs_ removed =
  foldr -$ ([], childs_) -$ removed $ \rem (remChildKeys, childs_') ->
    let (key, childs_') = M.alterF -$ rem -$ childs_' $ \case
          Nothing -> error "Swallow: remove nonexistent child"
          Just key -> (key, Nothing)
     in ((rem, key) : remChildKeys, childs_')

removeChilds ::
  [(Out.Self, IdMap.Key SingleParent.Self)] ->
  IO ()
removeChilds remChildKeys =
  for_ remChildKeys \(child, key) -> do
    let Out.Self{Out.implementation = Out.And And.Self{And.singleParent = sing}} = child
    SingleParent.removeListener key sing

addChilds ::
  M.HashMap Out.Self (IdMap.Key SingleParent.Self) ->
  S.HashSet Out.Self ->
  IO
    ( M.HashMap Out.Self (IdMap.Key SingleParent.Self)
    , S.HashSet Out.Self
    , S.HashSet Out.Self
    )
addChilds childs_ added =
  foldM -$ (childs_, S.empty, S.empty) -$ added $ \old@(childs_', add, rem) -> \case
    child@Out.Self
      { Out.implementation =
        Out.And
          And.Self
            { And.singleParent = singleParent
            , And.children = children
            }
      } -> do
        SingleParent.state (Out.triggerer child) >>= \case
          0 -> do
            grandchilds <- Children.state children
            return (childs_', foldr S.insert add grandchilds, S.insert child rem)
          _ -> do
            key <- SingleParent.addListener -$ singleParent $ \key -> do
              undefined -- in children, replace the child with its children
              SingleParent.removeListener key singleParent
            return (M.insert child key childs_', add, rem)
    _ -> return old

triggerFromChildren ::
  Children.Message ->
  Self ->
  IO (Maybe (S.HashSet Out.Self, S.HashSet Out.Self))
triggerFromChildren (Children.Remove _ _ removed) Self{childs} = do
  childs_ <- readIORef childs
  let (remChildKeys, childs_') = childsToRemove childs_ removed
  writeIORef childs childs_'
  removeChilds remChildKeys
  return Nothing
triggerFromChildren (Children.Replace _ _ added removed) Self{childs} = do
  childs_ <- readIORef childs
  let (remChildKeys, childs_') = childsToRemove childs_ removed
  (childs_'', add, rem) <- addChilds childs_' added
  writeIORef childs childs_''
  removeChilds remChildKeys
  return if S.null rem then Nothing else Just (add, rem)
triggerFromChildren (Children.Add _ _ added) Self{childs} = do
  childs_ <- readIORef childs
  (childs_', add, rem) <- addChilds childs_ added
  writeIORef childs childs_'
  return if S.null rem then Nothing else Just (add, rem)
