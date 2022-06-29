module Data.SimplifiedFormula.Agents.Swallow where

import qualified Data.HashSet as S
import Data.IORef
import qualified Data.SimplifiedFormula.Agents.Out as Out
import qualified Data.SimplifiedFormula.Utils.IdMap as IdMap

data Message = Replace Out.Self (S.HashSet Out.Self)

type Listener = Message -> IO ()
