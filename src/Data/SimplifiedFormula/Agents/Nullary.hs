{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}

module Data.SimplifiedFormula.Agents.Nullary where

import qualified Data.HashSet as S
import qualified Data.SimplifiedFormula.Agents.Children as Children
import qualified Data.SimplifiedFormula.Agents.Out as Out

trigger :: Children.Message -> Bool
trigger Children.Message{..} = S.null newState
