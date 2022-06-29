{-# LANGUAGE LambdaCase #-}

-- | a & !a = 0
module Data.SimplifiedFormula.Agents.Null where

import qualified Data.HashSet as S
import qualified Data.SimplifiedFormula.Agents.Children as Children
import qualified Data.SimplifiedFormula.Agents.Out as Out

type Listener = IO ()

step :: Bool -> Out.Env -> Out.Self -> IO Bool -> IO Bool
step isOr outEnv x b =
  b >>= \case
    True -> return True
    False ->
      Out.state x outEnv >>= \case
        Just (Out.Eval b) | b == isOr -> return True
        _ -> return False

triggerFromRemoved :: Bool -> S.HashSet Out.Self -> Out.Env -> IO Bool
triggerFromRemoved isOr removed outEnv = S.foldr (step isOr outEnv) (pure False) removed

trigger :: Bool -> Children.Message -> Out.Env -> IO Bool
trigger isOr (Children.Remove _ _ removed) outEnv = do
  triggerFromRemoved isOr removed outEnv
trigger isOr (Children.Replace _ _ removed _) outEnv =
  triggerFromRemoved isOr removed outEnv
trigger _ _ _ = return False

state :: Bool -> Children.Self -> Out.Env -> IO Bool
state isOr children outEnv = do
  childs <- Children.state children
  S.foldr (step isOr outEnv) (pure False) childs
