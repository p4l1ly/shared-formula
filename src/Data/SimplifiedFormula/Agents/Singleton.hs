{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}

module Data.SimplifiedFormula.Agents.Singleton where

import qualified Data.SimplifiedFormula.Agents.Children as Children
import qualified Data.SimplifiedFormula.Agents.Out as Out
import qualified Data.SimplifiedFormula.Utils.HashSet as S

type Message = Out.Self
type Listener = Message -> IO ()

trigger :: Children.Message -> Listener -> IO ()
trigger (Children.newState -> newState) listener =
  case S.sizeUpTo 2 newState of
    1 -> listener (S.peak newState)
    _ -> return ()

state :: Children.Self -> IO (Maybe Out.Self)
state children = do
  childs <- Children.state children
  return case S.sizeUpTo 2 childs of
    1 -> Just (S.peak childs)
    _ -> Nothing
