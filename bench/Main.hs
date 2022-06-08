{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LexicalNegation #-}

module Main (main) where

import Control.DeepSeq
import qualified Data.HashSet as S
import qualified Data.SimplifiedFormula.Utils.HashSet as S
import Data.Time.Clock.POSIX
import Test.Syd

main :: IO ()
main = sydTest $ do
  describe "sizeUpTo" do
    it "returns exact size if smaller than max" do
      S.sizeUpTo 100 (S.fromList [1 :: Int .. 50]) `shouldBe` 50
    it "returns exact size if greater than max" do
      S.sizeUpTo 30 (S.fromList [1 :: Int .. 50]) `shouldBe` 30
    it "is O(log(n)) if max is constant" do
      let measure n = deepseq set $ do
            tic <- getPOSIXTime
            S.sizeUpTo 2 set `shouldBe` 2
            (- tic) <$> getPOSIXTime
            where
              set = S.fromList [1 :: Int .. n]

      t1 <- measure 100
      t2 <- measure 100000
      t3 <- measure 1000000
      t2 / t1 < 100 `shouldBe` True
      t3 / t1 < 100 `shouldBe` True
  describe "peak" do
    it "is O(log(n)) if max is constant" do
      let measure n = deepseq set $ do
            tic <- getPOSIXTime
            deepseq (S.peak set) $
              (- tic) <$> getPOSIXTime
            where
              set = S.fromList [1 :: Int .. n]

      t1 <- measure 100
      t2 <- measure 100000
      t3 <- measure 1000000
      t2 / t1 < 100 `shouldBe` True
      t3 / t1 < 100 `shouldBe` True
