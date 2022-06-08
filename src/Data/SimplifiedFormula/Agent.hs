{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Data.SimplifiedFormula.Agent where

import qualified Data.HashMap.Strict as M
import Data.Hashable
import Data.IORef
import qualified Data.Vector.Mutable as V
import GHC.Generics

data Agent t = Agent
  { parentRefCounts :: !(M.HashMap (Agent t) Int)
  , extRefCount :: !Int
  , subdata :: !t
  , agentId :: Integer
  }
  deriving (Show, Generic)

instance Eq (Agent t) where
  a == b = agentId a == agentId b

instance Hashable (Agent t) where
  hashWithSalt = hashUsing agentId
  hash = hash . agentId

newtype Env = Env (IORef Integer)

newEnv :: IO Env
newEnv = Env <$> newIORef 0

newExt :: t -> Env -> IO (Agent t)
newExt subdata (Env env) = do
  agentId <- readIORef env
  writeIORef env (agentId + 1)
  let extRefCount = 1
  let parentRefCounts = M.empty
  return Agent{..}
