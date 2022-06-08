{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}

module Data.SimplifiedFormula.Agents.Nullary where

import qualified Data.HashSet as S
import qualified Data.SimplifiedFormula.Agents.Children as Children
import qualified Data.SimplifiedFormula.Agents.Out as Out

type Listener = IO ()

trigger :: Children.Message -> Listener -> IO ()
trigger (Children.newState -> childs) listener
  | S.null childs = listener
  | otherwise = return ()

state :: Children.Self -> IO Bool
state children = do
  childs <- Children.state children
  return $ S.null childs
