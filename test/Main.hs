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
import qualified Data.SimplifiedFormula.Agents.ShareSet as ShareSet
import qualified Data.SimplifiedFormula.Agents.SingleParent as SingleParent
import qualified Data.SimplifiedFormula.Utils.IdMap as IdMap
import Test.Syd

varListener :: Out.Self -> Out.Env -> IO (IORef (Maybe Out.Message))
varListener out outEnv = do
  var <- newIORef Nothing
  Out.addListener -$ out $ \key msg -> do
    writeIORef var (Just msg)
    Out.removeListener key out outEnv
  return var

varListener' :: Out.Self -> Out.Env -> IO (IORef (Maybe Out.Message), IdMap.Key Out.Self)
varListener' out outEnv = do
  var <- newIORef Nothing
  key <- Out.addListener -$ out $ \key msg -> do
    writeIORef var (Just msg)
    Out.removeListener key out outEnv
  return (var, key)

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
      a <- Out.newLeaf env
      b <- Out.newLeaf env
      a == b `shouldBe` False

      Right (andAB, andABImpl) <- Out.newAnd' [a, b, b] env
      childs <- Children.state (AndAgent.children andABImpl)
      childs == S.fromList [a, b] `shouldBe` True

    it "redirects and evaluates" do
      env <- Out.newEnv
      a <- Out.newLeaf env
      b <- Out.newLeaf env
      c <- Out.newLeaf env

      Right andAB <- Out.newAnd [a, b] env
      andABVar <- varListener andAB env

      Right (andCAndAB, andCAndABImpl) <- Out.newAnd' [andAB, c] env

      Nothing <- Out.state andAB
      Nothing <- Out.state andCAndAB
      Nothing <- readIORef andABVar

      Out.triggerListeners (Out.Eval True) b

      0 <- Out.parentCountFull andAB
      Just (Out.Redirect a') <- readIORef andABVar
      a == a' `shouldBe` True
      Nothing <- Out.state andCAndAB
      childs <- Children.state (AndAgent.children andCAndABImpl)
      childs == S.fromList [a, c] `shouldBe` True

      Out.triggerListeners (Out.Eval False) c

      Just (Out.Eval False) <- Out.state andCAndAB
      return ()

    it "decrements references" do
      env <- Out.newEnv
      a <- Out.newLeaf env
      b <- Out.newLeaf env
      c <- Out.newLeaf env
      d <- Out.newLeaf env

      -- a & b
      Right andAB <- Out.newAnd [a, b] env
      (andABVar, andABKey) <- varListener' andAB env

      -- c & d
      Right andCD <- Out.newAnd [c, d] env
      (andCDVar, andCDKey) <- varListener' andCD env

      -- (a & b) & (c & d)
      Right andABCD <- Out.newAnd [andAB, andCD] env
      (andABCDVar, andABCDKey) <- varListener' andABCD env

      Out.parentCountFull a >>= (`shouldBe` 1)
      Out.parentCountFull b >>= (`shouldBe` 1)
      Out.parentCountFull c >>= (`shouldBe` 1)
      Out.parentCountFull d >>= (`shouldBe` 1)
      Out.parentCountFull andCD >>= (`shouldBe` 2)
      Out.parentCountFull andAB >>= (`shouldBe` 2)
      Out.parentCountFull andABCD >>= (`shouldBe` 1)

      Out.removeListener andABCDKey andABCD env

      Out.parentCountFull a >>= (`shouldBe` 1)
      Out.parentCountFull b >>= (`shouldBe` 1)
      Out.parentCountFull c >>= (`shouldBe` 1)
      Out.parentCountFull d >>= (`shouldBe` 1)
      Out.parentCountFull andAB >>= (`shouldBe` 1)
      Out.parentCountFull andCD >>= (`shouldBe` 1)
      Out.parentCountFull andABCD >>= (`shouldBe` 0)

      Out.removeListener andCDKey andCD env

      Out.parentCountFull a >>= (`shouldBe` 1)
      Out.parentCountFull b >>= (`shouldBe` 1)
      Out.parentCountFull c >>= (`shouldBe` 0)
      Out.parentCountFull d >>= (`shouldBe` 0)
      Out.parentCountFull andAB >>= (`shouldBe` 1)
      Out.parentCountFull andCD >>= (`shouldBe` 0)
      Out.parentCountFull andABCD >>= (`shouldBe` 0)

      Out.removeListener andABKey andAB env

      Out.parentCountFull a >>= (`shouldBe` 0)
      Out.parentCountFull b >>= (`shouldBe` 0)
      Out.parentCountFull c >>= (`shouldBe` 0)
      Out.parentCountFull d >>= (`shouldBe` 0)
      Out.parentCountFull andAB >>= (`shouldBe` 0)
      Out.parentCountFull andCD >>= (`shouldBe` 0)
      Out.parentCountFull andABCD >>= (`shouldBe` 0)

    it "is shared" do
      env <- Out.newEnv
      a <- Out.newLeaf env
      b <- Out.newLeaf env
      c <- Out.newLeaf env

      -- a & b
      Right andAB <- Out.newAnd [a, b] env
      (andABVar, andABKey) <- varListener' andAB env

      -- b & a
      Left (Out.Redirect andBA) <- Out.newAnd [b, a] env
      andBA `shouldBe` andAB

      -- b & c
      Right andBC <- Out.newAnd [b, c] env

      Out.triggerListeners (Out.Redirect a) c

      Just (Out.Redirect andBC') <- Out.state andBC
      andBC' `shouldBe` andAB

    it "swallows" do
      env <- Out.newEnv
      a <- Out.newLeaf env
      b <- Out.newLeaf env
      c <- Out.newLeaf env
      d <- Out.newLeaf env

      -- a & b
      Right andAB <- Out.newAnd [a, b] env
      (andABVar, andABKey) <- varListener' andAB env

      Out.parentCountFull andAB >>= (`shouldBe` 1)

      -- (a & b) & c
      Right andABC <- Out.newAnd [andAB, c] env

      Out.parentCountFull andAB >>= (`shouldBe` 2)

      -- (a & b) & c & d
      Right (andABCD, andABCDImpl) <- Out.newAnd' [andABC, d] env

      Children.state (AndAgent.children andABCDImpl)
        >>= (`shouldBe` S.fromList [andAB, c, d])
      Out.parentCountFull andABC >>= (`shouldBe` 0)
      Out.parentCountFull andAB >>= (`shouldBe` 2)

      Out.removeListener andABKey andAB env

      Children.state (AndAgent.children andABCDImpl)
        >>= (`shouldBe` S.fromList [a, b, c, d])
      Out.parentCountFull andAB >>= (`shouldBe` 0)
      Out.parentCountFull a >>= (`shouldBe` 1)
      Out.parentCountFull b >>= (`shouldBe` 1)
      Out.parentCountFull c >>= (`shouldBe` 1)
      Out.parentCountFull d >>= (`shouldBe` 1)
