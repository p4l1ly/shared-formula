{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Data.SimplifiedFormula.Agents.And where

import Control.Lens
import qualified Data.HashMap.Strict as M
import Data.IORef
import qualified Data.SimplifiedFormula.Agents.Children as Children
import qualified Data.SimplifiedFormula.Agents.Null as Null
import qualified Data.SimplifiedFormula.Agents.Nullary as Nullary
import qualified Data.SimplifiedFormula.Agents.Out as Out
import qualified Data.SimplifiedFormula.Agents.Singleton as Singleton

data StaticChildrenPendings = StaticChildrenPendings
  { _pendingSingleton :: !(Maybe Children.Message)
  , _pendingNullary :: !(Maybe Children.Message)
  , _pendingNull :: !(Maybe Children.Message)
  }
makeLenses ''StaticChildrenPendings

emptyStaticChildrenPendings :: StaticChildrenPendings
emptyStaticChildrenPendings = StaticChildrenPendings Nothing Nothing Nothing

addStaticChildrenMessage ::
  Children.Message -> StaticChildrenPendings -> StaticChildrenPendings
addStaticChildrenMessage msg StaticChildrenPendings{..} =
  StaticChildrenPendings
    { _pendingSingleton = Just $ maybe msg (<> msg) _pendingSingleton
    , _pendingNullary = Just $ maybe msg (<> msg) _pendingNullary
    , _pendingNull = Just $ maybe msg (<> msg) _pendingNull
    }

data Triggerer = Triggerer
  { staticChildrenPendings :: IORef StaticChildrenPendings
  }

data Self = Self
  { triggerer :: !Triggerer
  , children :: !Children.Self
  }

triggerFromChildren :: Triggerer -> Out.Triggerer -> Children.Message -> IO ()
triggerFromChildren trig@Triggerer{..} outTrig msg = do
  modifyIORef staticChildrenPendings (addStaticChildrenMessage msg)
  handleStaticChildrenPendings trig outTrig

handleStaticChildrenPendings :: Triggerer -> Out.Triggerer -> IO ()
handleStaticChildrenPendings
  trig@Triggerer{staticChildrenPendings}
  outTrig =
    do
      handleOne pendingSingleton pendingSingleton \msg ->
        Singleton.trigger msg \out' ->
          Out.triggerListeners (Out.Redirect out') outTrig

      handleOne pendingNullary pendingNullary \msg ->
        Nullary.trigger msg $
          Out.triggerListeners (Out.Eval True) outTrig

      handleOne pendingNull pendingNull \msg ->
        Null.trigger False msg $
          Out.triggerListeners (Out.Eval False) outTrig
    where
      handleOne lensG lensS handler = do
        scp <- readIORef staticChildrenPendings
        ($ scp ^. lensG) $ maybe (return ()) \msg -> do
          writeIORef staticChildrenPendings (set lensS Nothing scp)
          handler msg

new :: [Out.Self] -> Out.Triggerer -> IO (Maybe Self)
new childs outTrig = do
  staticChildrenPendings <- newIORef emptyStaticChildrenPendings
  let triggerer = Triggerer{..}
  (children, (hasFalse, _)) <-
    Children.new childs (triggerFromChildren triggerer outTrig)
  if hasFalse
    then return Nothing
    else return $ Just Self{..}

state :: Self -> IO (Maybe Out.Message)
state Self{..} = do
  Nullary.state children >>= \case
    True -> return $ Just $ Out.Eval True
    False ->
      Singleton.state children >>= \case
        Just out -> return $ Just (Out.Redirect out)
        Nothing ->
          Null.state False children >>= \case
            True -> return $ Just $ Out.Eval False
            False -> return Nothing
