module Data.SimplifiedFormula.Utils.Fold where

foldToFor_ ::
  (Applicative m) =>
  ((x -> m a -> m a) -> m () -> xs -> m ()) ->
  xs ->
  (x -> m b) ->
  m ()
foldToFor_ fold xs step = fold (\x b -> b <* step x) (pure ()) xs

foldToPeak :: ((x -> unused -> Maybe x) -> Maybe x -> xs -> Maybe x) -> xs -> Maybe x
foldToPeak fold = fold (\a _ -> Just a) Nothing

foldToLimitedSize ::
  ((a -> (Int -> Int) -> Int -> Int) -> (Int -> Int) -> fa -> Int -> Int) ->
  Int ->
  fa ->
  Int
foldToLimitedSize fold max xs =
  fold (\_ fn z -> if z == max then max else fn (z + 1)) id xs 0
