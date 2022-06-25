module Data.SimplifiedFormula.Agents.Out where

import qualified Data.SimplifiedFormula.Utils.IdMap as IdMap

data Message
  = Eval Bool
  | Redirect Self

type Listener = Message -> IO ()
data Self
data Triggerer

addListener :: (IdMap.Key Self -> Listener) -> Triggerer -> IO (IdMap.Key Self)
removeListener :: IdMap.Key Self -> Self -> IO ()
triggerer :: Self -> Triggerer
state :: Self -> IO (Maybe Message)
