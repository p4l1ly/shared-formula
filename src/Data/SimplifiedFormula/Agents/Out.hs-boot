module Data.SimplifiedFormula.Agents.Out where

import Data.IORef
import {-# SOURCE #-} qualified Data.SimplifiedFormula.Agents.ShareSet as ShareSet
import qualified Data.SimplifiedFormula.Utils.IdMap as IdMap

data Message
  = Eval Bool
  | Redirect Self

type Listener = Message -> IO ()
data Self
data Triggerer
data Env = Env
  { andShareEnv :: ShareSet.EnvRef
  , nextId :: IORef Integer
  }

addListener :: (IdMap.Key Self -> Listener) -> Triggerer -> IO (IdMap.Key Self)
removeListener :: IdMap.Key Self -> Self -> Env -> IO ()
triggerer :: Self -> Triggerer
state :: Self -> Env -> IO (Maybe Message)
parentCount :: Triggerer -> IO Int
