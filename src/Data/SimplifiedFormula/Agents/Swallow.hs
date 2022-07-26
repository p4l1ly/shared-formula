{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Data.SimplifiedFormula.Agents.Swallow where

import Control.Monad
import Control.Monad.Trans.Except
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
  foldr -$ ([], childs_) -$ removed $ \rem (remChildKeys, childs_) ->
    let (key, childs_') = M.alterF -$ rem -$ childs_ $ \case
          Nothing -> (Nothing, Nothing)
          Just key -> (Just key, Nothing)
     in case key of
          Nothing -> (remChildKeys, childs_)
          Just key -> ((rem, key) : remChildKeys, childs_')

removeChilds ::
  [(Out.Self, IdMap.Key SingleParent.Self)] ->
  IO ()
removeChilds remChildKeys = do
  for_ remChildKeys \(child, key) -> do
    let Out.Self{Out.impl = Out.And And.Self{And.singleParent = sing}} = child
    SingleParent.removeListener key sing

addChilds ::
  M.HashMap Out.Self (IdMap.Key SingleParent.Self) ->
  Out.Env ->
  Out.Self ->
  And.Self ->
  S.HashSet Out.Self ->
  IO
    ( M.HashMap Out.Self (IdMap.Key SingleParent.Self)
    , S.HashSet Out.Self
    , S.HashSet Out.Self
    )
addChilds childs_ outEnv out andSelf@And.Self{children} added = do
  foldM -$ (childs_, S.empty, S.empty) -$ added $ \old@(childs_', add, rem) -> \case
    child@Out.Self
      { Out.impl =
        Out.And
          And.Self
            { And.singleParent = singleParent
            , And.children = grandchildren
            }
      } -> do
        SingleParent.state child >>= \case
          True -> do
            grandchilds <- Children.state grandchildren
            return (childs_', foldr S.insert add grandchilds, S.insert child rem)
          False -> do
            key <- SingleParent.addListener -$ singleParent $ \key -> do
              old <- Children.state children
              let new = S.delete child old
              let msg = Children.Message old new (S.singleton child) S.empty
              grandchilds <- Children.state grandchildren
              runExceptT do And.addChilds grandchilds outEnv out andSelf msg
                >>= And.finishTrigger out outEnv andSelf
              SingleParent.removeListener key singleParent
            return (M.insert child key childs_', add, rem)
    _ -> return old

triggerFromChildren ::
  Self ->
  Out.Env ->
  Out.Self ->
  And.Self ->
  Children.Message ->
  IO (S.HashSet Out.Self, S.HashSet Out.Self)
triggerFromChildren
  Self{..}
  outEnv
  out
  andSelf -- andSelf@And.Self{swallow = Self{..}}  causes a GHC bug
  (Children.Message _ _ minus plus) = do
    childs_ <- readIORef childs
    let (remChildKeys, childs_') = childsToRemove childs_ minus
    (childs_'', add, rem) <- addChilds childs_' outEnv out andSelf plus
    writeIORef childs childs_''
    removeChilds remChildKeys
    return (add, rem)

free :: Self -> IO ()
free Self{..} = readIORef childs >>= removeChilds . M.toList
