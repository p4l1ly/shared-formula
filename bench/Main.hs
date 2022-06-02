{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LexicalNegation #-}

module Main (main) where

import Control.DeepSeq
import Data.Formula.NAry.Shared (sizeUpTo)
import qualified Data.IntSet as IS
import Data.Time.Clock.POSIX
import Test.Syd

main :: IO ()
main = sydTest $
  describe "sizeUpTo" do
    it "returns exact size if smaller than max" do
      sizeUpTo 100 (IS.fromDistinctAscList [1 .. 50]) `shouldBe` 50
    it "returns exact size if greater than max" do
      sizeUpTo 30 (IS.fromDistinctAscList [1 .. 50]) `shouldBe` 30
    it "is O(log(n)) if max is constant" do
      let measure n = deepseq set $ do
            tic <- getPOSIXTime
            sizeUpTo 2 set `shouldBe` 2
            (- tic) <$> getPOSIXTime
            where set = IS.fromDistinctAscList [1 .. n]

      t1 <- measure 100
      t2 <- measure 100000
      t3 <- measure 1000000
      t2 / t1 < 100 `shouldBe` True
      t3 / t1 < 100 `shouldBe` True
