{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Data.SimplifiedFormula.Agents.ShareSet where

import Data.Function.Apply
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Data.IORef
import qualified Data.SimplifiedFormula.Agents.Children as Children
import qualified Data.SimplifiedFormula.Agents.Out as Out

type Env = M.HashMap (S.HashSet Out.Self) Out.Self
newtype EnvRef = EnvRef (IORef Env)
type Message = Out.Self
type Listener = Message -> IO ()

newEnv :: IO EnvRef
newEnv = EnvRef <$> newIORef M.empty

init :: Children.Message -> Out.Self -> EnvRef -> IO (Maybe Out.Self)
init Children.Message{Children.newState} out (EnvRef envRef) = do
  env <- readIORef envRef
  let (out', !env') = M.alterF -$ newState -$ env $ \case
        Nothing -> (Nothing, Just out)
        Just out' -> (Just out', Just out')
  writeIORef envRef env'
  return out'

free :: Children.Self -> EnvRef -> IO ()
free children (EnvRef envRef) = do
  env <- readIORef envRef
  childs <- Children.state children
  -- it could be pure but this is convenient for error strictness
  env' <- M.alterF -$ childs -$ env $ \case
    Nothing -> error "ShareSet: change without old value"
    Just out -> return Nothing
  writeIORef envRef env'

trigger :: Children.Message -> EnvRef -> IO (Maybe Out.Self)
trigger msg@Children.Message{..} (EnvRef envRef) = do
  env <- readIORef envRef
  let result = M.lookup newState env
  case result of
    Just out -> return ()
    Nothing -> do
      let (out, env') = M.alterF -$ oldState -$ env $ \case
            Nothing -> error $ "ShareSet: change without old value: " ++ show msg
            Just out -> (out, Nothing)
      writeIORef envRef (M.insert newState out env')
  return result
