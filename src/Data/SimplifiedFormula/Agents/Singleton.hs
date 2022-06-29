{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}

module Data.SimplifiedFormula.Agents.Singleton where

import qualified Data.HashSet as S
import qualified Data.SimplifiedFormula.Agents.Children as Children
import qualified Data.SimplifiedFormula.Agents.Out as Out
import qualified Data.SimplifiedFormula.Utils.HashSet as S

type Message = Out.Self
type Listener = Message -> IO ()

fromChilds :: S.HashSet a -> Maybe a
fromChilds childs = case S.sizeUpTo 2 childs of
  1 -> Just (S.peak childs)
  _ -> Nothing

trigger :: Children.Message -> Maybe Out.Self
trigger (Children.Remove _ newChilds _) = fromChilds newChilds
trigger (Children.Replace _ newChilds _ _) = fromChilds newChilds
trigger _ = Nothing

state :: Children.Self -> IO (Maybe Out.Self)
state children = do
  childs <- Children.state children
  return case S.sizeUpTo 2 childs of
    1 -> Just (S.peak childs)
    _ -> Nothing
