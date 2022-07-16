{-# LANGUAGE LambdaCase #-}
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

state :: Children.Self -> EnvRef -> IO (Maybe Out.Self)
state children (EnvRef envRef) = do
  M.lookup
    <$> Children.state children
    <*> readIORef envRef

add :: Children.Self -> Out.Self -> EnvRef -> IO ()
add children out (EnvRef envRef) = do
  childs <- Children.state children
  modifyIORef' envRef (M.insert childs out)

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
trigger msg (EnvRef envRef) = do
  let oldState = Children.oldState msg
  let newState = Children.newState msg
  env <- readIORef envRef
  let result = M.lookup newState env
  case result of
    Just out -> return ()
    Nothing -> do
      let (out, env') = M.alterF -$ oldState -$ env $ \case
            Nothing -> error "ShareSet: change without old value"
            Just out -> (out, Nothing)
      writeIORef envRef (M.insert newState out env')
  return result
