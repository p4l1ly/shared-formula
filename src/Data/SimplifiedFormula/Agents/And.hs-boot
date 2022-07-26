module Data.SimplifiedFormula.Agents.And where

import Control.Monad.Trans.Except
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
type Trigger = ExceptT Out.Message IO Children.Message

new :: IO Self
init ::
  [Out.Self] ->
  Out.Env ->
  Out.Self ->
  Self ->
  IO (Maybe Out.Message)
onDecRef :: Self -> Int -> Out.Env -> IO ()
addChilds ::
  Foldable f =>
  f Out.Self ->
  Out.Env ->
  Out.Self ->
  Self ->
  Children.Message ->
  Trigger
finishTrigger ::
  Out.Self ->
  Out.Env ->
  Self ->
  Either Out.Message Children.Message ->
  IO ()
