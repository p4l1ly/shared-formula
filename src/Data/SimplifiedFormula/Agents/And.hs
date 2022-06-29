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

data StaticChildrenPendings = StaticChildrenPendings
  { pendingSingleton :: !(Maybe Children.Message)
  , pendingNullary :: !(Maybe Children.Message)
  , pendingNull :: !(Maybe Children.Message)
  , pendingShare :: !(Maybe Children.Message)
  }

emptyStaticChildrenPendings :: StaticChildrenPendings
emptyStaticChildrenPendings = StaticChildrenPendings Nothing Nothing Nothing Nothing

addStaticChildrenMessage ::
  Children.Message -> StaticChildrenPendings -> StaticChildrenPendings
addStaticChildrenMessage msg StaticChildrenPendings{..} =
  StaticChildrenPendings
    { pendingSingleton = Just $ maybe msg (<> msg) pendingSingleton
    , pendingNullary = Just $ maybe msg (<> msg) pendingNullary
    , pendingNull = Just $ maybe msg (<> msg) pendingNull
    , pendingShare = Just $ maybe msg (<> msg) pendingShare
    }

data Triggerer = Triggerer
  { staticChildrenPendings :: IORef StaticChildrenPendings
  }

data Self = Self
  { triggerer :: !Triggerer
  , children :: !Children.Self
  , singleParent :: !SingleParent.Self
  }

triggerFromChildren :: Triggerer -> Out.Triggerer -> Out.Env -> Children.Message -> IO ()
triggerFromChildren trig@Triggerer{..} outTrig outEnv msg = do
  modifyIORef staticChildrenPendings (addStaticChildrenMessage msg)
  handleStaticChildrenPendings trig outTrig outEnv

handleStaticChildrenPendings :: Triggerer -> Out.Triggerer -> Out.Env -> IO ()
handleStaticChildrenPendings
  trig@Triggerer{staticChildrenPendings}
  outTrig
  outEnv =
    outMsg >>= \case
      Nothing -> return ()
      Just msg -> Out.triggerListeners msg outTrig
    where
      handleOne get clear handler = do
        scp <- readIORef staticChildrenPendings
        ($ get scp) $ maybe (return Nothing) \msg -> do
          writeIORef staticChildrenPendings (clear scp)
          handler msg
      outMsg = mconcat <$> sequence agentHandlers
      agentHandlers =
        [ handleOne pendingSingleton (\x -> x{pendingSingleton = Nothing}) \msg ->
            return $ Singleton.trigger msg <&> Out.Redirect
        , handleOne pendingNullary (\x -> x{pendingNullary = Nothing}) \msg ->
            return $ if Nullary.trigger msg then (Just $ Out.Eval True) else Nothing
        , handleOne pendingNull (\x -> x{pendingNull = Nothing}) \msg ->
            Null.trigger False msg outEnv <&> \case
              True -> Just $ Out.Eval False
              False -> Nothing
        , handleOne pendingShare (\x -> x{pendingShare = Nothing}) \msg ->
            ShareSet.trigger msg (Out.andShareEnv outEnv) <&> fmap Out.Redirect
        ]

new :: [Out.Self] -> Out.Triggerer -> Out.Env -> IO (Maybe Self)
new childs outTrig outEnv = do
  staticChildrenPendings <- newIORef emptyStaticChildrenPendings
  let triggerer = Triggerer{..}
  children <- Children.new
  hFlush stdout
  let childrenListener = triggerFromChildren triggerer outTrig outEnv
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
