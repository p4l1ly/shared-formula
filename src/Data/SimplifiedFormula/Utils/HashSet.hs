module Data.SimplifiedFormula.Utils.HashSet where

import qualified Data.HashSet as S

sizeUpTo :: Int -> S.HashSet a -> Int
sizeUpTo max xs = S.foldr (\_ fn z -> if z == max then max else fn (z + 1)) id xs 0

peak :: S.HashSet a -> a
peak = S.foldr (\a _ -> a) (error "peak from empty IntSet")
