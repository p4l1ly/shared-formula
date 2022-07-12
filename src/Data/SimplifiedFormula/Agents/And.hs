{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Data.SimplifiedFormula.Agents.And where

import Data.Functor
import qualified Data.HashMap.Strict as M
import Data.IORef
import qualified Data.SimplifiedFormula.Agents.Children as Children
import qualified Data.SimplifiedFormula.Agents.Null as Null
import qualified Data.SimplifiedFormula.Agents.Nullary as Nullary
import qualified Data.SimplifiedFormula.Agents.Out as Out
import qualified Data.SimplifiedFormula.Agents.ShareSet as ShareSet
import qualified Data.SimplifiedFormula.Agents.SingleParent as SingleParent
import qualified Data.SimplifiedFormula.Agents.Singleton as Singleton
import System.IO

data Self = Self
  { children :: !Children.Self
  , singleParent :: !SingleParent.Self
  }

triggerFromChildren :: Out.Triggerer -> Out.Env -> Children.Message -> IO ()
triggerFromChildren outTrig outEnv msg = do
  outMsg >>= \case
    Nothing -> return ()
    Just msg -> Out.triggerListeners msg outTrig
  where
    outMsg :: IO (Maybe Out.Message)
    outMsg = do
      shareMsg <- ShareSet.trigger msg (Out.andShareEnv outEnv) <&> fmap Out.Redirect
      Null.trigger False msg outEnv >>= \case
        True -> return $ Just $ Out.Eval False
        False
          | Nullary.trigger msg -> return $ Just $ Out.Eval True
          | otherwise ->
              case Singleton.trigger msg of
                Just out' -> return $ Just $ Out.Redirect out'
                Nothing -> return shareMsg

new :: [Out.Self] -> Out.Triggerer -> Out.Env -> IO (Maybe Self)
new childs outTrig outEnv = do
  children <- Children.new
  hFlush stdout
  let childrenListener = triggerFromChildren outTrig outEnv
  let process [] = do
        singleParent <- SingleParent.new
        return $ Just Self{..}
      process (child : childs') = do
        Children.addChild children outEnv child >>= \case
          Children.Present -> process childs'
          Children.Eval True -> process childs'
          Children.Eval False -> Nothing <$ Children.free children outEnv
          Children.Added child' -> do
            Children.confirmAddChild children outEnv childrenListener child'
            process childs'
  process childs

state :: Self -> Out.Env -> IO (Maybe Out.Message)
state Self{..} outEnv = do
  Nullary.state children >>= \case
    True -> return $ Just $ Out.Eval True
    False ->
      Singleton.state children >>= \case
        Just out -> return $ Just (Out.Redirect out)
        Nothing ->
          Null.state False children outEnv >>= \case
            True -> return $ Just $ Out.Eval False
            False ->
              ShareSet.state children (Out.andShareEnv outEnv) >>= \case
                Just out -> return $ Just (Out.Redirect out)
                Nothing -> return Nothing

onDecRef :: Self -> Int -> Out.Env -> IO ()
onDecRef Self{..} 0 outEnv = do
  -- WARN: Free ShareSet only after handling the Children's message.
  -- It relies on having the fresh children state hashed.
  ShareSet.free children (Out.andShareEnv outEnv)
  Children.free children outEnv
onDecRef Self{..} 1 outEnv = SingleParent.triggerListeners singleParent
onDecRef _ _ _ = return ()
