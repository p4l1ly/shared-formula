{-# LANGUAGE BlockArguments #-}

module Main (main) where

import Control.Monad
import Data.Formula.NAry.Shared (Term (..))
import qualified Data.Formula.NAry.Shared as Formula
import Data.Function.Apply
import qualified Data.HashSet as S
import Data.IORef
import qualified Data.IntSet as IS
import Data.List
import qualified Data.SimplifiedFormula.Agents.And as AndAgent
import qualified Data.SimplifiedFormula.Agents.Children as Children
import qualified Data.SimplifiedFormula.Agents.Out as Out
import Test.Syd

main :: IO ()
main = sydTest $ do
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

      Right orAB <- Formula.add (Or [a, b]) graph
      orAB `shouldBe` Formula.RefIx 2

      Right andBOrAB <- Formula.add (And [orAB, b]) graph
      andBOrAB `shouldBe` Formula.RefIx 1

      freeList <- readIORef (Formula.freeList graph)
      sort freeList `shouldBe` [Formula.RefIx 0, Formula.RefIx 2]

    it "absorbs OR into AND subsequently" do
      graph <- Formula.empty 1

      Right a <- Formula.add (Leaf "a") graph
      Formula.incRefExternal a graph
      a `shouldBe` Formula.RefIx 0
      Right b <- Formula.add (Leaf "b") graph
      b `shouldBe` Formula.RefIx 1
      Right c <- Formula.add (Leaf "c") graph
      c `shouldBe` Formula.RefIx 2

      Right orAB <- Formula.add (Or [a, b]) graph
      Formula.incRefExternal orAB graph
      orAB `shouldBe` Formula.RefIx 3

      Right orABC <- Formula.add (Or [orAB, c]) graph
      orABC `shouldBe` Formula.RefIx 4

      Right andAOrABC <- Formula.add (And [a, orABC]) graph
      andAOrABC `shouldBe` Formula.RefIx 5

      Right d <- Formula.add (Leaf "d") graph
      d `shouldBe` Formula.RefIx 6

      Right orDAndAOrABC <- Formula.add (Or [d, andAOrABC]) graph
      orDAndAOrABC `shouldBe` Formula.RefIx 7

      Formula.decRefExternal orAB graph

      freeList <- readIORef (Formula.freeList graph)
      sort freeList `shouldBe` map Formula.RefIx [1 .. 5]

      rootContents <- Formula.contents <$> Formula.getRef orDAndAOrABC graph
      rootContents `shouldBe` Formula.IOr (IS.fromList [0, 6])

    it "evaluates (a AND b AND !a) = 1, (a OR b OR !a) = 0" do
      graph <- Formula.empty 1

      Right a <- Formula.add (Leaf "a") graph
      Formula.incRefExternal a graph
      Formula.incRefExternal a graph
      a `shouldBe` Formula.RefIx 0
      Right b <- Formula.add (Leaf "b") graph
      Formula.incRefExternal b graph
      b `shouldBe` Formula.RefIx 1

      Right na <- Formula.add (Not a) graph
      Formula.incRefExternal na graph
      na `shouldBe` Formula.RefIx 2

      Left False <- Formula.add (And [a, b, na]) graph
      Left True <- Formula.add (Or [a, b, na]) graph
      return ()

    it "evaluates (a AND (b AND !a)) = 1, (a OR (b OR !a)) = 0" do
      graph <- Formula.empty 1

      Right a <- Formula.add (Leaf "a") graph
      Formula.incRefExternal a graph
      Formula.incRefExternal a graph
      a `shouldBe` Formula.RefIx 0
      Right b <- Formula.add (Leaf "b") graph
      Formula.incRefExternal b graph
      b `shouldBe` Formula.RefIx 1

      Right na <- Formula.add (Not a) graph
      Formula.incRefExternal na graph
      na `shouldBe` Formula.RefIx 2

      Right andBNotA <- Formula.add (And [b, na]) graph
      Formula.incRefExternal andBNotA graph
      andBNotA `shouldBe` Formula.RefIx 3

      Right orBNotA <- Formula.add (Or [b, na]) graph
      Formula.incRefExternal orBNotA graph
      orBNotA `shouldBe` Formula.RefIx 4

      Left False <- Formula.add (And [andBNotA, a]) graph
      Left True <- Formula.add (Or [orBNotA, a]) graph
      return ()

    it "law of common identities: a | (b & !a) = a | b" do
      graph <- Formula.empty 1

      Right a <- Formula.add (Leaf "a") graph
      Formula.incRefExternal a graph
      a `shouldBe` Formula.RefIx 0
      Right b <- Formula.add (Leaf "b") graph
      b `shouldBe` Formula.RefIx 1

      Right na <- Formula.add (Not a) graph
      na `shouldBe` Formula.RefIx 2

      Right andBNotA <- Formula.add (And [b, na]) graph
      andBNotA `shouldBe` Formula.RefIx 3

      Right orAAndBNotA <- Formula.add (Or [a, andBNotA]) graph
      orAAndBNotA `shouldBe` Formula.RefIx 3

      rootContents <- Formula.contents <$> Formula.getRef orAAndBNotA graph
      rootContents `shouldBe` Formula.IOr (IS.fromList [0, 1])

    it "law of common identities: a | (b & c & !a) = a | (b & c)" do
      graph <- Formula.empty 1

      Right a <- Formula.add (Leaf "a") graph
      Formula.incRefExternal a graph
      a `shouldBe` Formula.RefIx 0
      Right b <- Formula.add (Leaf "b") graph
      b `shouldBe` Formula.RefIx 1
      Right c <- Formula.add (Leaf "c") graph
      c `shouldBe` Formula.RefIx 2

      Right na <- Formula.add (Not a) graph
      na `shouldBe` Formula.RefIx 3

      Right andBCNotA <- Formula.add (And [b, c, na]) graph
      andBCNotA `shouldBe` Formula.RefIx 4

      Right orAAndBCNotA <- Formula.add (Or [a, andBCNotA]) graph
      orAAndBCNotA `shouldBe` Formula.RefIx 3

      rootContents <- Formula.contents <$> Formula.getRef orAAndBCNotA graph
      rootContents `shouldBe` Formula.IOr (IS.fromList [0, 4])

      andBCContents <- Formula.contents <$> Formula.getRef andBCNotA graph
      andBCContents `shouldBe` Formula.IAnd (IS.fromList [1, 2])

    it "law of common identities: a & (b | !a) = a & b" do
      graph <- Formula.empty 1

      Right a <- Formula.add (Leaf "a") graph
      Formula.incRefExternal a graph
      a `shouldBe` Formula.RefIx 0
      Right b <- Formula.add (Leaf "b") graph
      b `shouldBe` Formula.RefIx 1

      Right na <- Formula.add (Not a) graph
      na `shouldBe` Formula.RefIx 2

      Right andBNotA <- Formula.add (Or [b, na]) graph
      andBNotA `shouldBe` Formula.RefIx 3

      Right orAOrBNotA <- Formula.add (And [a, andBNotA]) graph
      orAOrBNotA `shouldBe` Formula.RefIx 3

      rootContents <- Formula.contents <$> Formula.getRef orAOrBNotA graph
      rootContents `shouldBe` Formula.IAnd (IS.fromList [0, 1])

    it "law of common identities: a & (b | c | !a) = a & (b | c)" do
      graph <- Formula.empty 1

      Right a <- Formula.add (Leaf "a") graph
      Formula.incRefExternal a graph
      a `shouldBe` Formula.RefIx 0
      Right b <- Formula.add (Leaf "b") graph
      b `shouldBe` Formula.RefIx 1
      Right c <- Formula.add (Leaf "c") graph
      c `shouldBe` Formula.RefIx 2

      Right na <- Formula.add (Not a) graph
      na `shouldBe` Formula.RefIx 3

      Right andBCNotA <- Formula.add (Or [b, c, na]) graph
      andBCNotA `shouldBe` Formula.RefIx 4

      Right orAOrBCNotA <- Formula.add (And [a, andBCNotA]) graph
      orAOrBCNotA `shouldBe` Formula.RefIx 3

      rootContents <- Formula.contents <$> Formula.getRef orAOrBCNotA graph
      rootContents `shouldBe` Formula.IAnd (IS.fromList [0, 4])

      andBCContents <- Formula.contents <$> Formula.getRef andBCNotA graph
      andBCContents `shouldBe` Formula.IOr (IS.fromList [1, 2])

  describe "AndAgent" do
    it "builds" do
      env <- Out.newEnv
      aMsg <- newIORef Nothing
      bMsg <- newIORef Nothing
      aTrig <- Out.newTriggerer
      bTrig <- Out.newTriggerer
      a <- Out.new aTrig (Out.Leaf aMsg) env
      b <- Out.new bTrig (Out.Leaf bMsg) env
      a == b `shouldBe` False

      outTrig <- Out.newTriggerer
      Just andAB <- AndAgent.new [a, b, b] outTrig
      Nothing <- AndAgent.state andAB
      childs <- Children.state (AndAgent.children andAB)
      childs == S.fromList [a, b] `shouldBe` True

    it "redirects to 'a' if 'b' is True" do
      env <- Out.newEnv
      aMsg <- newIORef Nothing
      bMsg <- newIORef (Just $ Out.Eval True)
      aTrig <- Out.newTriggerer
      bTrig <- Out.newTriggerer
      a <- Out.new aTrig (Out.Leaf aMsg) env
      b <- Out.new bTrig (Out.Leaf bMsg) env

      outTrig <- Out.newTriggerer
      Just andAB <- AndAgent.new [a, b] outTrig
      Just (Out.Redirect a') <- AndAgent.state andAB
      a == a' `shouldBe` True

    it "evaluates if 'b' is False" do
      env <- Out.newEnv
      aMsg <- newIORef Nothing
      bMsg <- newIORef (Just $ Out.Eval False)
      aTrig <- Out.newTriggerer
      bTrig <- Out.newTriggerer
      a <- Out.new aTrig (Out.Leaf aMsg) env
      b <- Out.new bTrig (Out.Leaf bMsg) env

      outTrig <- Out.newTriggerer
      Nothing <- AndAgent.new [a, b] outTrig
      return ()

    it "leaves trigger redirection, then evaluation" do
      env <- Out.newEnv
      aMsg <- newIORef Nothing
      bMsg <- newIORef Nothing
      cMsg <- newIORef Nothing
      aTrig <- Out.newTriggerer
      bTrig <- Out.newTriggerer
      cTrig <- Out.newTriggerer
      a <- Out.new aTrig (Out.Leaf aMsg) env
      b <- Out.new bTrig (Out.Leaf bMsg) env
      c <- Out.new cTrig (Out.Leaf cMsg) env

      andABTrig <- Out.newTriggerer
      andABMsg <- newIORef Nothing
      Out.addListener -$ andABTrig $ \_ msg -> writeIORef andABMsg $ Just msg
      Just andAB <- AndAgent.new [a, b] andABTrig
      andABOut <- Out.new andABTrig (Out.And andAB) env

      andCAndABTrig <- Out.newTriggerer
      andCAndABMsg <- newIORef Nothing
      Out.addListener -$ andCAndABTrig $ \_ msg -> writeIORef andCAndABMsg $ Just msg
      Just andCAndAB <- AndAgent.new [andABOut, c] andCAndABTrig

      Nothing <- AndAgent.state andAB
      Nothing <- AndAgent.state andCAndAB
      Nothing <- readIORef andABMsg
      Nothing <- readIORef andCAndABMsg

      writeIORef bMsg (Just $ Out.Eval True)
      Out.triggerListeners (Out.Eval True) bTrig

      Just (Out.Redirect a') <- AndAgent.state andAB
      a == a' `shouldBe` True
      Just (Out.Redirect a') <- readIORef andABMsg
      a == a' `shouldBe` True
      Nothing <- AndAgent.state andCAndAB
      Nothing <- readIORef andCAndABMsg
      childs <- Children.state (AndAgent.children andCAndAB)
      childs == S.fromList [a, c] `shouldBe` True

      writeIORef cMsg (Just $ Out.Eval False)
      Out.triggerListeners (Out.Eval False) cTrig

      Just (Out.Eval False) <- readIORef andCAndABMsg
      return ()
