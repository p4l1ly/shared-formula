{-# LANGUAGE BlockArguments #-}

module Main (main) where

import Control.Monad
import Data.Formula.NAry.Shared (Term (..))
import qualified Data.Formula.NAry.Shared as Formula
import Data.IORef
import qualified Data.IntSet as IS
import Data.List
import Test.Syd

main :: IO ()
main = sydTest $
  describe "simplify" do
    it "removes double negation" do
      graph <- Formula.empty 1

      Right a <- Formula.add (Leaf "a") graph
      a `shouldBe` Formula.RefIx 0
      Right notA <- Formula.add (Not a) graph
      notA `shouldBe` Formula.RefIx 1
      Right notNotA <- Formula.add (Not notA) graph
      notNotA `shouldBe` Formula.RefIx 0

    it "flattens ANDs" do
      graph <- Formula.empty 1

      Right a <- Formula.add (Leaf "a") graph
      Formula.incRefExternal a graph
      a `shouldBe` Formula.RefIx 0
      Right b <- Formula.add (Leaf "b") graph
      Formula.incRefExternal b graph
      b `shouldBe` Formula.RefIx 1
      Right c <- Formula.add (Leaf "c") graph
      Formula.incRefExternal c graph
      c `shouldBe` Formula.RefIx 2

      Right andAB <- Formula.add (And [a, b]) graph
      andAB `shouldBe` Formula.RefIx 3

      Right andABC <- Formula.add (And [andAB, c]) graph
      andABC `shouldBe` Formula.RefIx 3
      andABCContents <- Formula.contents <$> Formula.getRef andABC graph
      andABCContents `shouldBe` Formula.IAnd (IS.fromList [0, 1, 2])

      Right andABC2 <- Formula.add (And [a, b, c]) graph
      andABC2 `shouldBe` Formula.RefIx 3

      freeList <- readIORef (Formula.freeList graph)
      sort freeList `shouldBe` []

    it "flattens ORs" do
      graph <- Formula.empty 1

      Right a <- Formula.add (Leaf "a") graph
      Formula.incRefExternal a graph
      a `shouldBe` Formula.RefIx 0
      Right b <- Formula.add (Leaf "b") graph
      Formula.incRefExternal b graph
      b `shouldBe` Formula.RefIx 1
      Right c <- Formula.add (Leaf "c") graph
      Formula.incRefExternal c graph
      c `shouldBe` Formula.RefIx 2

      Right orAB <- Formula.add (Or [a, b]) graph
      orAB `shouldBe` Formula.RefIx 3

      Right orABC <- Formula.add (Or [orAB, c]) graph
      orABC `shouldBe` Formula.RefIx 3
      orABCContents <- Formula.contents <$> Formula.getRef orABC graph
      orABCContents `shouldBe` Formula.IOr (IS.fromList [0, 1, 2])

      Right orABC2 <- Formula.add (Or [a, b, c]) graph
      orABC2 `shouldBe` Formula.RefIx 3

      freeList <- readIORef (Formula.freeList graph)
      sort freeList `shouldBe` []

    it "flattens ANDs subsequently" do
      graph <- Formula.empty 1

      Right a <- Formula.add (Leaf "a") graph
      Formula.incRefExternal a graph
      a `shouldBe` Formula.RefIx 0
      Right b <- Formula.add (Leaf "b") graph
      Formula.incRefExternal b graph
      b `shouldBe` Formula.RefIx 1
      Right c <- Formula.add (Leaf "c") graph
      Formula.incRefExternal c graph
      c `shouldBe` Formula.RefIx 2

      Right andAB <- Formula.add (And [a, b]) graph
      andAB `shouldBe` Formula.RefIx 3
      Formula.incRefExternal andAB graph

      Right andABC <- Formula.add (And [andAB, c]) graph
      andABC `shouldBe` Formula.RefIx 4
      andABCContents <- Formula.contents <$> Formula.getRef andABC graph
      andABCContents `shouldBe` Formula.IAnd (IS.fromList [3, 2])

      Formula.decRefExternal andAB graph
      andABCContents <- Formula.contents <$> Formula.getRef andABC graph
      andABCContents `shouldBe` Formula.IAnd (IS.fromList [0, 1, 2])

      Right andABC2 <- Formula.add (And [a, b, c]) graph
      andABC2 `shouldBe` Formula.RefIx 4

      freeList <- readIORef (Formula.freeList graph)
      sort freeList `shouldBe` [Formula.RefIx 3]

    it "flattens ORs subsequently" do
      graph <- Formula.empty 1

      Right a <- Formula.add (Leaf "a") graph
      Formula.incRefExternal a graph
      a `shouldBe` Formula.RefIx 0
      Right b <- Formula.add (Leaf "b") graph
      Formula.incRefExternal b graph
      b `shouldBe` Formula.RefIx 1
      Right c <- Formula.add (Leaf "c") graph
      Formula.incRefExternal c graph
      c `shouldBe` Formula.RefIx 2

      Right orAB <- Formula.add (Or [a, b]) graph
      orAB `shouldBe` Formula.RefIx 3
      Formula.incRefExternal orAB graph

      Right orABC <- Formula.add (Or [orAB, c]) graph
      orABC `shouldBe` Formula.RefIx 4
      orABCContents <- Formula.contents <$> Formula.getRef orABC graph
      orABCContents `shouldBe` Formula.IOr (IS.fromList [3, 2])

      Formula.decRefExternal orAB graph
      orABCContents <- Formula.contents <$> Formula.getRef orABC graph
      orABCContents `shouldBe` Formula.IOr (IS.fromList [0, 1, 2])

      Right orABC2 <- Formula.add (Or [a, b, c]) graph
      orABC2 `shouldBe` Formula.RefIx 4

      freeList <- readIORef (Formula.freeList graph)
      sort freeList `shouldBe` [Formula.RefIx 3]

    it "finds sharing" do
      graph <- Formula.empty 1
      Right a <- Formula.add (Leaf "a") graph
      a `shouldBe` Formula.RefIx 0
      Right a2 <- Formula.add (Leaf "a") graph
      a2 `shouldBe` Formula.RefIx 0
      Right b <- Formula.add (Leaf "b") graph
      b `shouldBe` Formula.RefIx 1

    it "does not flatten ANDs to ORs" do
      graph <- Formula.empty 1

      Right a <- Formula.add (Leaf "a") graph
      a `shouldBe` Formula.RefIx 0
      Right b <- Formula.add (Leaf "b") graph
      b `shouldBe` Formula.RefIx 1
      Right c <- Formula.add (Leaf "c") graph
      c `shouldBe` Formula.RefIx 2

      Right andAB <- Formula.add (And [a, b]) graph
      andAB `shouldBe` Formula.RefIx 3

      Right orCAndAB <- Formula.add (Or [andAB, c]) graph
      orCAndAB `shouldBe` Formula.RefIx 4
      orCAndABContents <- Formula.contents <$> Formula.getRef orCAndAB graph
      orCAndABContents `shouldBe` Formula.IOr (IS.fromList [3, 2])

    it "does not flatten ANDs to ORs subsequently" do
      graph <- Formula.empty 1

      Right a <- Formula.add (Leaf "a") graph
      a `shouldBe` Formula.RefIx 0
      Right b <- Formula.add (Leaf "b") graph
      b `shouldBe` Formula.RefIx 1
      Right c <- Formula.add (Leaf "c") graph
      c `shouldBe` Formula.RefIx 2

      Right andAB <- Formula.add (And [a, b]) graph
      andAB `shouldBe` Formula.RefIx 3
      Formula.incRefExternal andAB graph

      Right orCAndAB <- Formula.add (Or [andAB, c]) graph
      orCAndAB `shouldBe` Formula.RefIx 4
      orCAndABContents <- Formula.contents <$> Formula.getRef orCAndAB graph
      orCAndABContents `shouldBe` Formula.IOr (IS.fromList [3, 2])

      Formula.decRefExternal andAB graph
      orCAndABContents' <- Formula.contents <$> Formula.getRef orCAndAB graph
      orCAndABContents' `shouldBe` Formula.IOr (IS.fromList [3, 2])

    it "absorbs AND into OR" do
      graph <- Formula.empty 1

      Right a <- Formula.add (Leaf "a") graph
      a `shouldBe` Formula.RefIx 0
      Right b <- Formula.add (Leaf "b") graph
      Formula.incRefExternal b graph
      b `shouldBe` Formula.RefIx 1

      Right andAB <- Formula.add (And [a, b]) graph
      andAB `shouldBe` Formula.RefIx 2

      Right orBAndAB <- Formula.add (Or [andAB, b]) graph
      orBAndAB `shouldBe` Formula.RefIx 1

      freeList <- readIORef (Formula.freeList graph)
      sort freeList `shouldBe` [Formula.RefIx 0, Formula.RefIx 2]

    it "absorbs OR into AND" do
      graph <- Formula.empty 1

      Right a <- Formula.add (Leaf "a") graph
      a `shouldBe` Formula.RefIx 0
      Right b <- Formula.add (Leaf "b") graph
      Formula.incRefExternal b graph
      b `shouldBe` Formula.RefIx 1

      Right andAB <- Formula.add (Or [a, b]) graph
      andAB `shouldBe` Formula.RefIx 2

      Right andBOrAB <- Formula.add (And [andAB, b]) graph
      andBOrAB `shouldBe` Formula.RefIx 1

      freeList <- readIORef (Formula.freeList graph)
      sort freeList `shouldBe` [Formula.RefIx 0, Formula.RefIx 2]
