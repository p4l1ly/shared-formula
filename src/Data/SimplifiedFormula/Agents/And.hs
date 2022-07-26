{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Data.SimplifiedFormula.Agents.And where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Foldable
import Data.Functor
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.Maybe
import qualified Data.SimplifiedFormula.Agents.Children as Children
import qualified Data.SimplifiedFormula.Agents.Null as Null
import qualified Data.SimplifiedFormula.Agents.Nullary as Nullary
import qualified Data.SimplifiedFormula.Agents.Out as Out
import qualified Data.SimplifiedFormula.Agents.ShareSet as ShareSet
import qualified Data.SimplifiedFormula.Agents.SingleParent as SingleParent
import qualified Data.SimplifiedFormula.Agents.Singleton as Singleton
import qualified Data.SimplifiedFormula.Agents.Swallow as Swallow

data Self = Self
  { children :: !Children.Self
  , singleParent :: !SingleParent.Self
  , swallow :: !Swallow.Self
  }

type Trigger = ExceptT Out.Message IO Children.Message

triggerNull :: Out.Env -> Children.Message -> Trigger
triggerNull outEnv msg = ExceptT do
  Null.trigger msg <&> \case
    True -> Left (Out.Eval False)
    False -> Right msg

triggerNullary :: Children.Message -> Trigger
triggerNullary msg
  | Nullary.trigger msg = throwE (Out.Eval True)
  | otherwise = return msg

triggerSingleton :: Children.Message -> Trigger
triggerSingleton msg = ExceptT do
  return $ maybe (Right msg) (Left . Out.Redirect) (Singleton.trigger msg)

triggerShareSet :: Out.Env -> Children.Message -> Trigger
triggerShareSet outEnv msg = ExceptT do
  ShareSet.trigger msg (Out.andShareEnv outEnv)
    <&> maybe (Right msg) (Left . Out.Redirect)

initShareSet :: Out.Env -> Out.Self -> Children.Message -> Trigger
initShareSet outEnv out msg = ExceptT do
  ShareSet.init msg out (Out.andShareEnv outEnv)
    <&> maybe (Right msg) (Left . Out.Redirect)

triggerSwallow ::
  Out.Self ->
  Out.Env ->
  Self ->
  Children.Message ->
  Trigger
triggerSwallow out outEnv self@Self{..} msg@Children.Message{..} = ExceptT do
  (add, rem) <- Swallow.triggerFromChildren swallow outEnv out self msg
  if S.null add && S.null rem
    then return (Right msg)
    else do
      let againMsg = Children.Message newState newState S.empty S.empty
      let againMsg' = foldr Children.remChild againMsg rem
      runExceptT $ do
        addChilds0 add outEnv againMsg'
          >>= triggerNullary
          >>= triggerSingleton
          >>= triggerSwallow out outEnv self
          <&> (msg <>)

finishTrigger ::
  Out.Self ->
  Out.Env ->
  Self ->
  Either Out.Message Children.Message ->
  IO ()
finishTrigger out outEnv self (Left outMsg) = do
  Out.triggerListeners outMsg out
finishTrigger out outEnv self@Self{..} (Right childrenMsg) = do
  Children.apply children outEnv childrenListener' childrenMsg
  Children.triggerListeners children outEnv childrenMsg
  where
    childrenListener' = childrenListener out outEnv self

childrenListener ::
  Out.Self ->
  Out.Env ->
  Self ->
  Children.Message ->
  IO ()
childrenListener out outEnv self =
  finishTrigger out outEnv self
    <=< runExceptT . triggerFromChildren out outEnv self

triggerFromChildren ::
  Out.Self ->
  Out.Env ->
  Self ->
  Children.Message ->
  Trigger
triggerFromChildren out outEnv self msg = do
  triggerNull outEnv msg
    >>= triggerNullary
    >>= triggerSingleton
    >>= triggerSwallow out outEnv self
    >>= triggerShareSet outEnv

addChilds ::
  Foldable f =>
  f Out.Self ->
  Out.Env ->
  Out.Self ->
  Self ->
  Children.Message ->
  Trigger
addChilds childs outEnv out self msg =
  addChilds0 childs outEnv msg
    >>= triggerNullary
    >>= triggerSingleton
    >>= triggerSwallow out outEnv self
    >>= triggerShareSet outEnv

addChilds0 ::
  Foldable f =>
  f Out.Self ->
  Out.Env ->
  Children.Message ->
  Trigger
addChilds0 childs outEnv msg = foldM process msg childs
  where
    process msg child = ExceptT do
      Children.addChild outEnv child msg <&> Null.onAddChild msg

new :: IO Self
new = do
  children <- Children.new
  swallow <- Swallow.new
  singleParent <- SingleParent.new
  return Self{..}

init ::
  [Out.Self] ->
  Out.Env ->
  Out.Self ->
  Self ->
  IO (Maybe Out.Message)
init childs outEnv out self@Self{..} = do
  runExceptT do
    addChilds0 childs outEnv msg
      >>= triggerNullary
      >>= triggerSingleton
      >>= triggerSwallow out outEnv self
      >>= initShareSet outEnv out
    >>= \case
      Left outMsg -> return $ Just outMsg
      Right childrenMsg -> do
        let childrenListener' = childrenListener out outEnv self
        Children.apply children outEnv childrenListener' childrenMsg
        for_ childs (Out.freeIfZeroParents outEnv)
        return Nothing
  where
    msg = Children.Message S.empty S.empty S.empty S.empty

onDecRef :: Self -> Int -> Out.Env -> IO ()
onDecRef Self{..} 0 outEnv = do
  ShareSet.free children (Out.andShareEnv outEnv)
  Swallow.free swallow
  Children.free children outEnv
onDecRef Self{..} 1 outEnv = do
  SingleParent.triggerListeners singleParent
onDecRef _ _ _ = return ()
