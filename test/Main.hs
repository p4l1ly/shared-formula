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
import Test.Syd

varListener :: Out.Self -> Out.Env -> IO (IORef (Maybe Out.Message))
varListener out outEnv = do
  var <- newIORef Nothing
  Out.addListener -$ out $ \key msg -> do
    writeIORef var (Just msg)
    Out.removeListener key out outEnv
  return var

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
      Just andAB <- AndAgent.new [a, b, b] outTrig env
      Nothing <- AndAgent.state andAB env
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
      Just andAB <- AndAgent.new [a, b] outTrig env
      Just (Out.Redirect a') <- AndAgent.state andAB env
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
      Nothing <- AndAgent.new [a, b] outTrig env
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
      Just andAB <- AndAgent.new [a, b] andABTrig env
      andABOut <- Out.new andABTrig (Out.And andAB) env
      andABKey <- Out.addListener (\_ msg -> return ()) andABOut
      andABMsg <- varListener andABOut env
      ShareSet.add (AndAgent.children andAB) andABOut (Out.andShareEnv env)

      andCAndABTrig <- Out.newTriggerer
      Just andCAndAB <- AndAgent.new [andABOut, c] andCAndABTrig env
      andCAndABOut <- Out.new andCAndABTrig (Out.And andCAndAB) env
      andCAndABMsg <- varListener andCAndABOut env
      ShareSet.add (AndAgent.children andCAndAB) andCAndABOut (Out.andShareEnv env)

      Nothing <- Out.state andABOut env
      Nothing <- Out.state andCAndABOut env
      Nothing <- readIORef andABMsg
      Nothing <- readIORef andCAndABMsg

      writeIORef bMsg (Just $ Out.Eval True)
      Out.triggerListeners (Out.Eval True) bTrig

      1 <- Out.parentCountFull (Out.triggerer andABOut)
      Just (Out.Redirect a') <- readIORef andABMsg
      a == a' `shouldBe` True
      Nothing <- Out.state andCAndABOut env
      Nothing <- readIORef andCAndABMsg
      childs <- Children.state (AndAgent.children andCAndAB)
      childs == S.fromList [a, c] `shouldBe` True

      writeIORef cMsg (Just $ Out.Eval False)
      Out.triggerListeners (Out.Eval False) cTrig

      Just (Out.Eval False) <- readIORef andCAndABMsg
      return ()

    it "decRefs" do
      env <- Out.newEnv
      aMsg <- newIORef Nothing
      bMsg <- newIORef Nothing
      cMsg <- newIORef Nothing
      dMsg <- newIORef Nothing
      aTrig <- Out.newTriggerer
      bTrig <- Out.newTriggerer
      cTrig <- Out.newTriggerer
      dTrig <- Out.newTriggerer
      a <- Out.new aTrig (Out.Leaf aMsg) env
      b <- Out.new bTrig (Out.Leaf bMsg) env
      c <- Out.new cTrig (Out.Leaf cMsg) env
      d <- Out.new dTrig (Out.Leaf dMsg) env

      -- a & b
      andABTrig <- Out.newTriggerer
      Just andAB <- AndAgent.new [a, b] andABTrig env
      andABOut <- Out.new andABTrig (Out.And andAB) env
      andABKey <- Out.addListener (\_ msg -> return ()) andABOut
      ShareSet.add (AndAgent.children andAB) andABOut (Out.andShareEnv env)

      -- c & d
      andCDTrig <- Out.newTriggerer
      Just andCD <- AndAgent.new [c, d] andCDTrig env
      andCDOut <- Out.new andCDTrig (Out.And andCD) env
      andCDKey <- Out.addListener (\_ msg -> return ()) andCDOut
      ShareSet.add (AndAgent.children andCD) andCDOut (Out.andShareEnv env)

      -- (a & b) & (c & d)
      andABCDTrig <- Out.newTriggerer
      Just andABCD <- AndAgent.new [andABOut, andCDOut] andABCDTrig env
      andABCDOut <- Out.new andABCDTrig (Out.And andABCD) env
      andABCDKey <- Out.addListener (\_ msg -> return ()) andABCDOut
      ShareSet.add (AndAgent.children andABCD) andABCDOut (Out.andShareEnv env)

      Out.parentCountFull aTrig >>= (`shouldBe` 1)
      Out.parentCountFull bTrig >>= (`shouldBe` 1)
      Out.parentCountFull cTrig >>= (`shouldBe` 1)
      Out.parentCountFull dTrig >>= (`shouldBe` 1)
      Out.parentCountFull andCDTrig >>= (`shouldBe` 2)
      Out.parentCountFull andABTrig >>= (`shouldBe` 2)
      Out.parentCountFull andABCDTrig >>= (`shouldBe` 1)

      Out.removeListener andABCDKey andABCDOut env

      Out.parentCountFull aTrig >>= (`shouldBe` 1)
      Out.parentCountFull bTrig >>= (`shouldBe` 1)
      Out.parentCountFull cTrig >>= (`shouldBe` 1)
      Out.parentCountFull dTrig >>= (`shouldBe` 1)
      Out.parentCountFull andABTrig >>= (`shouldBe` 1)
      Out.parentCountFull andCDTrig >>= (`shouldBe` 1)
      Out.parentCountFull andABCDTrig >>= (`shouldBe` 0)

      Out.removeListener andCDKey andCDOut env

      Out.parentCountFull aTrig >>= (`shouldBe` 1)
      Out.parentCountFull bTrig >>= (`shouldBe` 1)
      Out.parentCountFull cTrig >>= (`shouldBe` 0)
      Out.parentCountFull dTrig >>= (`shouldBe` 0)
      Out.parentCountFull andABTrig >>= (`shouldBe` 1)
      Out.parentCountFull andCDTrig >>= (`shouldBe` 0)
      Out.parentCountFull andABCDTrig >>= (`shouldBe` 0)

      Out.removeListener andABKey andABOut env

      Out.parentCountFull aTrig >>= (`shouldBe` 0)
      Out.parentCountFull bTrig >>= (`shouldBe` 0)
      Out.parentCountFull cTrig >>= (`shouldBe` 0)
      Out.parentCountFull dTrig >>= (`shouldBe` 0)
      Out.parentCountFull andABTrig >>= (`shouldBe` 0)
      Out.parentCountFull andCDTrig >>= (`shouldBe` 0)
      Out.parentCountFull andABCDTrig >>= (`shouldBe` 0)

    it "is shared" do
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
      Just andAB <- AndAgent.new [a, b] andABTrig env
      Nothing <- AndAgent.state andAB env
      andABOut <- Out.new andABTrig (Out.And andAB) env
      ShareSet.add (AndAgent.children andAB) andABOut (Out.andShareEnv env)

      andABTrig' <- Out.newTriggerer
      Just andAB' <- AndAgent.new [b, a] andABTrig' env
      Just _ <- AndAgent.state andAB' env

      andBCTrig <- Out.newTriggerer
      Just andBC <- AndAgent.new [b, c] andBCTrig env
      Nothing <- AndAgent.state andBC env
      andBCOut <- Out.new andBCTrig (Out.And andBC) env
      andBCMsg <- varListener andBCOut env
      ShareSet.add (AndAgent.children andBC) andBCOut (Out.andShareEnv env)

      writeIORef cMsg (Just $ Out.Redirect a)
      Out.triggerListeners (Out.Redirect a) cTrig

      Just (Out.Redirect andBCOut') <- readIORef andBCMsg
      andBCOut' == andABOut `shouldBe` True

    it "swallows" do
      env <- Out.newEnv
      aMsg <- newIORef Nothing
      bMsg <- newIORef Nothing
      cMsg <- newIORef Nothing
      dMsg <- newIORef Nothing
      aTrig <- Out.newTriggerer
      bTrig <- Out.newTriggerer
      cTrig <- Out.newTriggerer
      dTrig <- Out.newTriggerer
      a <- Out.new aTrig (Out.Leaf aMsg) env
      b <- Out.new bTrig (Out.Leaf bMsg) env
      c <- Out.new cTrig (Out.Leaf cMsg) env
      d <- Out.new dTrig (Out.Leaf dMsg) env

      andABTrig <- Out.newTriggerer
      Just andAB <- AndAgent.new [a, b] andABTrig env
      Nothing <- AndAgent.state andAB env
      andABOut <- Out.new andABTrig (Out.And andAB) env
      andABKey <- Out.addListener (\_ msg -> return ()) andABOut
      ShareSet.add (AndAgent.children andAB) andABOut (Out.andShareEnv env)

      Out.parentCountFull andABTrig >>= (`shouldBe` 1)

      andABCTrig <- Out.newTriggerer
      Just andABC <- AndAgent.new [andABOut, c] andABCTrig env
      Nothing <- AndAgent.state andABC env
      andABCOut <- Out.new andABCTrig (Out.And andABC) env
      ShareSet.add (AndAgent.children andABC) andABCOut (Out.andShareEnv env)

      Out.parentCountFull andABTrig >>= (`shouldBe` 2)

      andABCDTrig <- Out.newTriggerer
      Just andABCD <- AndAgent.new [andABCOut, d] andABCDTrig env
      Nothing <- AndAgent.state andABCD env
      andABCDOut <- Out.new andABCDTrig (Out.And andABCD) env
      ShareSet.add (AndAgent.children andABCD) andABCDOut (Out.andShareEnv env)

      Children.state (AndAgent.children andABCD)
        >>= (`shouldBe` S.fromList [andABOut, c, d])
      Out.parentCountFull andABTrig >>= (`shouldBe` 2)

      Out.removeListener andABKey andABOut env

      Children.state (AndAgent.children andABCD)
        >>= (`shouldBe` S.fromList [a, b, c, d])
      Out.parentCountFull andABTrig >>= (`shouldBe` 0)
      Out.parentCountFull aTrig >>= (`shouldBe` 1)
      Out.parentCountFull bTrig >>= (`shouldBe` 1)
      Out.parentCountFull cTrig >>= (`shouldBe` 1)
      Out.parentCountFull dTrig >>= (`shouldBe` 1)
