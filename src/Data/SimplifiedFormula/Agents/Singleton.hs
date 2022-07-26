{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Data.SimplifiedFormula.Agents.Singleton where

import qualified Data.HashSet as S
import qualified Data.SimplifiedFormula.Agents.Children as Children
import qualified Data.SimplifiedFormula.Agents.Out as Out
import qualified Data.SimplifiedFormula.Utils.HashSet as S

type Message = Out.Self
type Listener = Message -> IO ()

trigger :: Children.Message -> Maybe Out.Self
trigger Children.Message{..} = case S.sizeUpTo 2 newState of
  1 -> Just (S.peak newState)
  _ -> Nothing
