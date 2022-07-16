{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Data.SimplifiedFormula.Agents.And where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
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
  }

triggerFromChildren ::
  Out.Triggerer -> Out.Env -> Children.Self -> Children.Message -> IO ()
triggerFromChildren outTrig outEnv children msg = do
  Null.trigger False msg outEnv >>= \case
    True -> Out.triggerListeners (Out.Eval False) outTrig
    False
      | Nullary.trigger msg -> Out.triggerListeners (Out.Eval True) outTrig
      | otherwise ->
          case Singleton.trigger msg of
            Just out' -> Out.triggerListeners (Out.Redirect out') outTrig
            Nothing -> do
              ShareSet.trigger msg (Out.andShareEnv outEnv) >>= \case
                Just out' -> Out.triggerListeners (Out.Redirect out') outTrig
                Nothing -> do
                  Children.apply
                    children
                    outEnv
                    (triggerFromChildren outTrig outEnv children)
                    msg
                  Children.triggerListeners children outEnv msg

new :: [Out.Self] -> Out.Triggerer -> Out.Env -> IO (Maybe Self)
new childs outTrig outEnv = do
  children <- Children.new
  let childrenListener = triggerFromChildren outTrig outEnv children
  let process [] msg = do
        singleParent <- SingleParent.new
        Children.apply children outEnv childrenListener msg
        return $ Just Self{..}
      process (child : childs') msg = do
        Children.addChild outEnv child msg >>= \case
          Children.Present -> process childs' msg
          Children.Eval True -> process childs' msg
          Children.Eval False -> Nothing <$ Children.free children outEnv
          Children.Added child' msg' -> process childs' msg'
  process childs (Children.Init S.empty)

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
  -- WARN: Free ShareSet only after handling the Children's message.
  -- It relies on having the fresh children state hashed.
  ShareSet.free children (Out.andShareEnv outEnv)
  Children.free children outEnv
onDecRef Self{..} 1 outEnv = SingleParent.triggerListeners singleParent
onDecRef _ _ _ = return ()
