module Data.SimplifiedFormula.Utils.IntSet where

import qualified Data.IntSet as IS

sizeUpTo :: Int -> IS.IntSet -> Int
sizeUpTo max xs = IS.fold (\_ fn z -> if z == max then max else fn (z + 1)) id xs 0

peak :: IS.IntSet -> Int
peak = IS.fold (\i _ -> i) (error "peak from empty HashSet")
