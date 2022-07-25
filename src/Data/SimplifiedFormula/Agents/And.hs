{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Data.SimplifiedFormula.Agents.And where

import Control.Monad
import Control.Monad.Trans.Maybe
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

triggerNullFromAddChilds ::
  Out.Triggerer -> Maybe Children.Message -> IO (Maybe Children.Message)
triggerNullFromAddChilds outTrig msg = do
  case Null.triggerFromAddChilds msg of
    True -> Nothing <$ Out.triggerListeners (Out.Eval False) outTrig
    False -> return msg

triggerNull ::
  Out.Triggerer -> Out.Env -> Children.Message -> IO (Maybe Children.Message)
triggerNull outTrig outEnv msg = do
  Null.trigger False msg outEnv >>= \case
    True -> Nothing <$ Out.triggerListeners (Out.Eval False) outTrig
    False -> return $ Just msg

triggerNullary :: Out.Triggerer -> Children.Message -> IO (Maybe Children.Message)
triggerNullary outTrig msg
  | Nullary.trigger msg = do
      Nothing <$ Out.triggerListeners (Out.Eval True) outTrig
  | otherwise = do
      return $ Just msg

triggerSingleton :: Out.Triggerer -> Children.Message -> IO (Maybe Children.Message)
triggerSingleton outTrig msg = do
  case Singleton.trigger msg of
    Just out' -> Nothing <$ Out.triggerListeners (Out.Redirect out') outTrig
    Nothing -> return $ Just msg

triggerShareSet :: Out.Triggerer -> Out.Env -> Children.Message -> IO (Maybe Children.Message)
triggerShareSet outTrig outEnv msg = do
  ShareSet.trigger msg (Out.andShareEnv outEnv) >>= \case
    Just out' -> Nothing <$ Out.triggerListeners (Out.Redirect out') outTrig
    Nothing -> return $ Just msg

triggerSwallow ::
  Out.Triggerer ->
  Out.Env ->
  Children.Self ->
  Swallow.Self ->
  Children.Message ->
  IO (Maybe Children.Message)
triggerSwallow outTrig outEnv children swallow msg@Children.Message{..} = do
  (add, rem) <- Swallow.triggerFromChildren swallow outEnv outTrig children msg
  if S.null add && S.null rem
    then return $ Just msg
    else do
      let againMsg = Children.Message newState newState S.empty S.empty
      msg' <- addChilds add outEnv (foldr Children.remChild againMsg rem)
      runMaybeT $
        MaybeT (triggerNullFromAddChilds outTrig msg')
          >>= MaybeT . triggerNullary outTrig
          >>= MaybeT . triggerSingleton outTrig
          >>= MaybeT . triggerSwallow outTrig outEnv children swallow
          <&> (fromJust msg' <>)

finishTrigger ::
  Out.Triggerer ->
  Out.Env ->
  Children.Self ->
  Swallow.Self ->
  Maybe Children.Message ->
  IO ()
finishTrigger outTrig outEnv children swallow Nothing = do
  return ()
finishTrigger outTrig outEnv children swallow (Just msg) = do
  let onMessage =
        finishTrigger outTrig outEnv children swallow
          <=< triggerFromChildren outTrig outEnv children swallow
  Children.apply children outEnv onMessage msg
  Children.triggerListeners children outEnv msg

triggerFromChildren ::
  Out.Triggerer ->
  Out.Env ->
  Children.Self ->
  Swallow.Self ->
  Children.Message ->
  IO (Maybe Children.Message)
triggerFromChildren outTrig outEnv children swallow msg = do
  runMaybeT $
    MaybeT (triggerNull outTrig outEnv msg)
      >>= MaybeT . triggerNullary outTrig
      >>= MaybeT . triggerSingleton outTrig
      >>= MaybeT . triggerSwallow outTrig outEnv children swallow
      >>= MaybeT . triggerShareSet outTrig outEnv

triggerFromAddChilds ::
  Out.Triggerer ->
  Out.Env ->
  Children.Self ->
  Swallow.Self ->
  Maybe Children.Message ->
  IO (Maybe Children.Message)
triggerFromAddChilds outTrig outEnv children swallow msg = do
  runMaybeT $
    MaybeT (triggerNullFromAddChilds outTrig msg)
      >>= MaybeT . triggerNullary outTrig
      >>= MaybeT . triggerSingleton outTrig
      >>= MaybeT . triggerSwallow outTrig outEnv children swallow
      >>= MaybeT . triggerShareSet outTrig outEnv

addChilds ::
  Foldable f =>
  f Out.Self ->
  Out.Env ->
  Children.Message ->
  IO (Maybe Children.Message)
addChilds childs outEnv msg = do
  foldr process (return . Just) childs msg
  where
    process child cont msg = do
      Children.addChild outEnv child msg >>= \case
        Children.Present -> cont msg
        Children.Eval True -> cont msg
        Children.Eval False -> return Nothing
        Children.Added child' msg' -> cont msg'

new :: [Out.Self] -> Out.Triggerer -> Out.Env -> IO (Maybe Self)
new childs0 outTrig outEnv = do
  children <- Children.new
  let rec :: Foldable f => f Out.Self -> Children.Message -> IO (Maybe Self)
      rec childs msg =
        addChilds childs outEnv msg >>= \case
          Nothing -> return Nothing
          Just msg -> do
            swallow <- Swallow.new
            singleParent <- SingleParent.new
            (add, rem) <- Swallow.triggerFromChildren swallow outEnv outTrig children msg
            if S.null add && S.null rem
              then do
                let childrenListener =
                      finishTrigger outTrig outEnv children swallow
                        <=< triggerFromChildren outTrig outEnv children swallow
                Children.apply children outEnv childrenListener msg
                let finalChilds = Children.newState msg
                for_ childs0 \child -> do
                  unless (S.member child finalChilds) do Out.pingRefCount child outEnv
                return $ Just Self{..}
              else rec add (foldr Children.remChild msg rem)
  rec childs0 (Children.Message S.empty S.empty S.empty S.empty)

state :: Self -> Out.Env -> IO (Maybe Out.Message)
state Self{..} outEnv = do
  Nullary.state children >>= \case
    True -> return $ Just $ Out.Eval True
    False ->
      Singleton.state children >>= \case
        Just out -> return $ Just $ Out.Redirect out
        Nothing ->
          Null.state False children outEnv >>= \case
            True -> return $ Just $ Out.Eval False
            False ->
              ShareSet.state children (Out.andShareEnv outEnv) >>= \case
                Just out -> return $ Just $ Out.Redirect out
                Nothing -> return Nothing

onDecRef :: Self -> Int -> Out.Env -> IO ()
onDecRef Self{..} 0 outEnv = do
  ShareSet.free children (Out.andShareEnv outEnv)
  Swallow.free swallow
  Children.free children outEnv
onDecRef Self{..} 1 outEnv = do
  SingleParent.triggerListeners singleParent
onDecRef _ _ _ = return ()
