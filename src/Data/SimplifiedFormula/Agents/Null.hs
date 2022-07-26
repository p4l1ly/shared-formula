{-# LANGUAGE LambdaCase #-}

-- | a & !a = 0
module Data.SimplifiedFormula.Agents.Null where

import qualified Data.HashSet as S
import Data.Maybe
import qualified Data.SimplifiedFormula.Agents.Children as Children
import qualified Data.SimplifiedFormula.Agents.Out as Out

type Listener = IO ()

step :: Out.Self -> IO Bool -> IO Bool
step x b =
  b >>= \case
    True -> return True
    False ->
      Out.state x >>= \case
        Just (Out.Eval False) -> return True
        _ -> return False

trigger :: Children.Message -> IO Bool
trigger (Children.Message _ _ minus _) = S.foldr step (pure False) minus

onAddChild ::
  Children.Message -> Children.AddChildResult -> Either Out.Message Children.Message
onAddChild msg = \case
  Children.Present -> Right msg
  Children.Eval True -> Right msg
  Children.Eval False -> Left $ Out.Eval False
  Children.Added child' msg' -> Right msg'
