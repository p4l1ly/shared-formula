module Data.SimplifiedFormula.Agents.And where

import {-# SOURCE #-} qualified Data.SimplifiedFormula.Agents.Out as Out
import qualified Data.SimplifiedFormula.Utils.IdMap as IdMap

data Self
data Triggerer

new :: [Out.Self] -> Out.Triggerer -> Out.Env -> IO (Maybe Self)
state :: Self -> Out.Env -> IO (Maybe Out.Message)
onDecRef :: Self -> Int -> Out.Env -> IO ()
