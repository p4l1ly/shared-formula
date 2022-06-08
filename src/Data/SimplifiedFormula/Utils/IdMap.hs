{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Data.SimplifiedFormula.Utils.IdMap where

import qualified Data.HashMap.Strict as M
import Data.Hashable
import GHC.Generics

newtype Key tag = Key Integer deriving (Show, Eq, Generic, Hashable)

idSucc :: Key tag -> Key tag
idSucc (Key x) = Key (succ x)

data IdMap tag elem = IdMap
  { next :: !(Key tag)
  , items :: !(M.HashMap (Key tag) elem)
  }

empty :: IdMap tag elem
empty = IdMap (Key 0) M.empty

add :: elem -> IdMap tag elem -> (Key tag, IdMap tag elem)
add elem IdMap{..} = (next, IdMap (idSucc next) (M.insert next elem items))

remove :: Key tag -> IdMap tag elem -> IdMap tag elem
remove key idmap@IdMap{items} = idmap{items = M.delete key items}

get :: Key tag -> IdMap tag elem -> elem
get key IdMap{items} = items M.! key

null :: IdMap tag elem -> Bool
null IdMap{items} = M.null items

foldElems :: (elem -> b -> b) -> b -> IdMap tag elem -> b
foldElems fn z IdMap{items} = M.foldr' fn z items
