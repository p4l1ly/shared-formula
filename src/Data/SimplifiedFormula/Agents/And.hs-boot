module Data.SimplifiedFormula.Agents.And where

import {-# SOURCE #-} qualified Data.SimplifiedFormula.Agents.Children as Children
import {-# SOURCE #-} qualified Data.SimplifiedFormula.Agents.Out as Out
import {-# SOURCE #-} qualified Data.SimplifiedFormula.Agents.SingleParent as SingleParent
import {-# SOURCE #-} qualified Data.SimplifiedFormula.Agents.Swallow as Swallow
import qualified Data.SimplifiedFormula.Utils.IdMap as IdMap

data Self = Self
  { children :: !Children.Self
  , singleParent :: !SingleParent.Self
  , swallow :: !Swallow.Self
  }

new :: [Out.Self] -> Out.Triggerer -> Out.Env -> IO (Maybe Self)
state :: Self -> Out.Env -> IO (Maybe Out.Message)
onDecRef :: Self -> Int -> Out.Env -> IO ()
addChilds ::
  Foldable f =>
  f Out.Self ->
  Out.Env ->
  Children.Message ->
  IO (Maybe Children.Message)
triggerFromAddChilds ::
  Out.Triggerer ->
  Out.Env ->
  Children.Self ->
  Swallow.Self ->
  Maybe Children.Message ->
  IO (Maybe Children.Message)
finishTrigger ::
  Out.Triggerer ->
  Out.Env ->
  Children.Self ->
  Swallow.Self ->
  Maybe Children.Message ->
  IO ()
