{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Formula.NAry.Shared where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Function.Apply ((-$))
import Data.Functor
import Data.Functor.Compose
import qualified Data.HashMap.Strict as M
import Data.Hashable
import Data.IORef
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import Data.List
import Data.Traversable
import qualified Data.Vector as V
import Data.Vector.Instances ()
import qualified Data.Vector.Mutable as V
import qualified Data.Vector.Mutable as VM
import GHC.Generics

newtype RefIx = RefIx {unRefIx :: Int}
  deriving (Show, Eq, Ord)
  deriving (Hashable) via Int

data Term t r
  = Not !r
  | And ![r]
  | Or ![r]
  | Leaf !t
  deriving (Show, Eq, Generic, Hashable, Functor, Foldable, Traversable)

data ITerm t
  = INot !RefIx
  | IAnd !IS.IntSet
  | IOr !IS.IntSet
  | ILeaf !t
  deriving (Show, Eq, Generic, Hashable)

data CountedRef t = CountedRef
  { contents :: !(ITerm t)
  , parentCount :: !Int
  , parents :: !IS.IntSet
  }
  deriving (Show)

data Graph t = Graph
  { tCons :: !(IORef (M.HashMap (ITerm t) RefIx))
  , refs :: !(IORef (V.IOVector (CountedRef t)))
  , refsLen :: !(IORef Int)
  , freeList :: !(IORef [RefIx])
  }

empty :: Int -> IO (Graph t)
empty reserve = do
  tCons <- newIORef M.empty
  refs <- V.new reserve >>= newIORef
  refsLen <- newIORef 0
  freeList <- newIORef []
  return Graph{..}

add :: Hashable t => Term t RefIx -> Graph t -> IO (Either Bool RefIx)
add term graph = do
  simplifyNew term graph >>= \case
    Result b -> return $ Left b
    Singleton refix -> return $ Right refix
    Formula term' -> do
      cons <- readIORef (tCons graph)
      (refix, cons') <- getCompose (M.alterF (alter term') term' cons)
      writeIORef (tCons graph) cons'
      return $ Right refix
  where
    alter _ (Just refix) = Compose do
      incRefExternal refix graph
      cref <- getRef refix graph
      itermFor_ (contents cref) \i -> decRefExternal (RefIx i) graph
      return (refix, Just refix)
    alter term Nothing = Compose do
      refix <-
        readIORef (freeList graph) >>= \case
          (refix : rest) -> do
            writeIORef (freeList graph) rest
            return refix
          [] -> do
            refs_ <- readIORef (refs graph)
            let capacity = VM.length refs_
            len <- readIORef (refsLen graph)
            writeIORef (refsLen graph) (len + 1)
            when (len == capacity) do
              refs_' <- VM.unsafeGrow refs_ (len + 1) -- we grow twice + 1
              writeIORef (refs graph) refs_'
            return (RefIx len)
      let cref =
            CountedRef
              { contents = term
              , parentCount = 1
              , parents = IS.empty
              }
      setRef refix cref graph
      onCreate refix graph
      return (refix, Just refix)

data Simplification t
  = Result Bool
  | Singleton RefIx
  | Formula (ITerm t)
  deriving (Eq, Show)

getRef :: RefIx -> Graph t -> IO (CountedRef t)
getRef (RefIx refix) graph =
  readIORef (refs graph) >>= VM.read -$ refix

setRef :: RefIx -> CountedRef t -> Graph t -> IO ()
setRef (RefIx refix) ref graph =
  readIORef (refs graph) >>= VM.write -$ refix -$ ref

modifyRef :: RefIx -> (CountedRef t -> CountedRef t) -> Graph t -> IO ()
modifyRef (RefIx refix) modif graph =
  readIORef (refs graph) >>= VM.modify -$ modif -$ refix

incRefExternal :: RefIx -> Graph t -> IO ()
incRefExternal child graph = do
  refs_ <- readIORef (refs graph)
  modifyRef child -$ graph $ \ref@CountedRef{parentCount} ->
    ref{parentCount = parentCount + 1}

incRefInternal :: RefIx -> RefIx -> Graph t -> IO ()
incRefInternal (RefIx parent) child graph = do
  modifyRef child -$ graph $ \ref@CountedRef{parentCount, parents} ->
    ref{parentCount = parentCount + 1, parents = IS.insert parent parents}

removeRef :: Hashable t => RefIx -> Graph t -> IO ()
removeRef refix graph = do
  term <- contents <$> getRef refix graph
  modifyIORef' (tCons graph) (M.delete term)
  modifyIORef' (freeList graph) (refix :)

data Change t
  = Evaluated Bool
  | Redirected RefIx
  | Changed (ITerm t)
  deriving (Show, Eq)

notifyParents :: Hashable t => RefIx -> Change t -> Graph t -> IO ()
notifyParents refix change graph = do
  CountedRef{parents} <- getRef refix graph
  forIntSet_ parents \(RefIx -> parentRefix) ->
    onChildChange parentRefix refix change graph

changeChildsAnd :: Hashable t => RefIx -> IS.IntSet -> Graph t -> IO ()
changeChildsAnd refix childs' graph = do
  node@CountedRef{contents = old@(IAnd childs), parents} <- getRef refix graph
  change <- case intSetDumbSize childs' of
    Zero ->
      -- Our deallocation will be handled by parents.
      return $ Evaluated True
    One ->
      -- Our deallocation will be handled by parents.
      return $ Redirected (RefIx $ head $ IS.toList childs')
    Many -> do
      -- We change ourselves *before* notifying the parents.
      -- This should be safe as our logical value should not change.
      setRef refix node{contents = IAnd childs'} graph
      return $ Changed old
  notifyParents refix change graph

changeChildsOr :: Hashable t => RefIx -> IS.IntSet -> Graph t -> IO ()
changeChildsOr refix childs' graph = do
  node@CountedRef{contents = old@(IOr childs), parents} <- getRef refix graph
  -- We've been evaluated, shrinked to a singleton or only generally changed.
  change <- case intSetDumbSize childs' of
    Zero ->
      -- Our deallocation will be handled by parents.
      return $ Evaluated False
    One ->
      -- Our deallocation will be handled by parents.
      return $ Redirected (RefIx $ head $ IS.toList childs')
    Many -> do
      -- We change ourselves *before* notifying the parents.
      -- This should be safe as our logical value should not change.
      setRef refix node{contents = IOr childs'} graph
      return $ Changed old
  notifyParents refix change graph

onChildSingleParent :: Hashable t => RefIx -> Graph t -> IO ()
onChildSingleParent childRefix@(RefIx childIx) graph = do
  child <- getRef childRefix graph
  case contents child of
    IAnd childChilds -> do
      forIntSet_ (parents child) \(RefIx -> parentRefix) -> do
        parent <- getRef parentRefix graph
        case contents parent of
          old@(IAnd parentChilds) -> do
            -- We will refer the grandchildren.
            forIntSet_ childChilds \i -> incRefInternal parentRefix (RefIx i) graph
            -- The child can be unreferred (and thus deallocated).
            decRefInternal parentRefix childRefix graph
            -- Let's remove the child and add the grandchildren.
            let parentChilds' = IS.delete childIx parentChilds
            parentChilds'' <- intSetFoldM parentChilds' childChilds \i uniqXs' ->
              swallowAnd graph (Just parentRefix) uniqXs' (RefIx i)
            changeChildsAnd parentRefix parentChilds'' graph
          _ -> return ()
    IOr childChilds ->
      forIntSet_ (parents child) \(RefIx -> parentRefix) -> do
        parent <- getRef parentRefix graph
        case contents parent of
          old@(IOr parentChilds) -> do
            -- We will refer the grandchildren.
            forIntSet_ childChilds \i -> incRefInternal parentRefix (RefIx i) graph
            -- The child can be unreferred (and thus deallocated).
            decRefInternal parentRefix childRefix graph
            -- Let's add the grandchildren.
            let parentChilds' = IS.delete childIx parentChilds
            parentChilds'' <- intSetFoldM parentChilds' childChilds \i uniqXs' ->
              swallowOr graph (Just parentRefix) uniqXs' (RefIx i)
            changeChildsOr parentRefix parentChilds'' graph
          _ -> return ()
    INot _ -> return ()
    ILeaf _ -> return ()

onChildChange :: Hashable t => RefIx -> RefIx -> Change t -> Graph t -> IO ()
onChildChange parentRefix childRefix@(RefIx childIx) change graph = case change of
  Evaluated b -> do
    decRefInternal parentRefix childRefix graph
    parent <- getRef parentRefix graph
    case contents parent of
      INot _ -> notifyParents parentRefix (Evaluated $ not b) graph
      IAnd parentChilds
        | b -> changeChildsAnd parentRefix (IS.delete childIx parentChilds) graph
        | otherwise -> notifyParents parentRefix (Evaluated False) graph
      IOr parentChilds
        | b -> notifyParents parentRefix (Evaluated True) graph
        | otherwise ->
          changeChildsOr parentRefix (IS.delete childIx parentChilds) graph
      ILeaf t -> error "ILeaf: onChildChange"
  Redirected childRefix'@(RefIx childIx') -> do
    incRefInternal parentRefix childRefix' graph
    decRefInternal parentRefix childRefix graph
    parent <- getRef parentRefix graph
    case contents parent of
      INot _ -> do
        swallowNot graph (Just parentRefix) childRefix' >>= \case
          Nothing ->
            setRef parentRefix parent{contents = INot childRefix'} graph
          Just parentRefix' ->
            notifyParents parentRefix (Redirected parentRefix') graph
      IAnd parentChilds -> do
        let parentChilds' = IS.delete childIx parentChilds
        parentChilds'' <-
          swallowAnd graph (Just parentRefix) parentChilds' (RefIx childIx')
        changeChildsAnd parentRefix parentChilds'' graph
      IOr parentChilds -> do
        let parentChilds' = IS.delete childIx parentChilds
        parentChilds'' <-
          swallowOr graph (Just parentRefix) parentChilds' (RefIx childIx')
        changeChildsOr parentRefix parentChilds'' graph
      ILeaf t -> error "ILeaf: onChildChange"
  Changed _ -> return ()

decRefExternal :: Hashable t => RefIx -> Graph t -> IO ()
decRefExternal refix graph = do
  modifyRef refix -$ graph $ \ref -> ref{parentCount = parentCount ref - 1}
  ref <- getRef refix graph
  case parentCount ref of
    0 -> removeRef refix graph
    1 -> onChildSingleParent refix graph
    _ -> return ()

decRefInternal :: Hashable t => RefIx -> RefIx -> Graph t -> IO ()
decRefInternal (RefIx parent) refix graph = do
  modifyRef refix -$ graph $ \ref ->
    ref{parentCount = parentCount ref - 1, parents = IS.delete parent (parents ref)}
  ref <- getRef refix graph
  case parentCount ref of
    0 -> removeRef refix graph
    1 -> onChildSingleParent refix graph
    _ -> return ()

onCreate :: RefIx -> Graph t -> IO ()
onCreate refix@(RefIx parent) graph = do
  cref <- getRef refix graph
  itermFor_ (contents cref) \i ->
    modifyRef (RefIx i) -$ graph $ \child@CountedRef{parents} ->
      child{parents = IS.insert parent parents}

forIntSet_ :: Applicative f => IS.IntSet -> (Int -> f b) -> f ()
forIntSet_ xs step = IS.fold (\i a -> a <* step i) (pure ()) xs

intSetFoldM :: Monad f => b -> IS.IntSet -> (Int -> b -> f b) -> f b
intSetFoldM init xs step = IS.fold (\i a -> a >>= step i) (pure init) xs

itermFor_ :: Applicative f => ITerm t -> (Int -> f b) -> f ()
itermFor_ (IAnd xs) step = forIntSet_ xs step
itermFor_ (IOr xs) step = forIntSet_ xs step
itermFor_ (INot (RefIx x)) step = () <$ step x
itermFor_ (ILeaf x) step = pure ()

itermFoldM :: Monad f => b -> ITerm t -> (Int -> b -> f b) -> f b
itermFoldM b (IAnd xs) step = intSetFoldM b xs step
itermFoldM b (IOr xs) step = intSetFoldM b xs step
itermFoldM b (INot (RefIx x)) step = step x b
itermFoldM b (ILeaf x) step = pure b

data DumbCount = Zero | One | Many deriving (Show, Eq, Ord)

dumbInc :: DumbCount -> DumbCount
dumbInc Zero = One
dumbInc One = Many
dumbInc Many = Many

intSetDumbSize :: IS.IntSet -> DumbCount
intSetDumbSize = IS.fold (const dumbInc) Zero -- TODO it's probably still O(n) but O(1) should be somehow possible

swallowAnd :: Hashable t => Graph t -> Maybe RefIx -> IS.IntSet -> RefIx -> IO IS.IntSet
swallowAnd graph parent uniqXs refix = do
  let rec uniqXs refix@(RefIx x)
        | IS.member x uniqXs = do
          decRef refix graph
          return uniqXs
        | otherwise = do
          getRef refix graph >>= \case
            CountedRef{contents = IAnd childXs, parentCount = 1} -> do
              forIntSet_ childXs \i -> incRef (RefIx i) graph
              decRef refix graph
              intSetFoldM uniqXs childXs \i uniqXs' -> rec uniqXs' (RefIx i)
            _ -> return (IS.insert x uniqXs)
  rec uniqXs refix
  where
    incRef = maybe incRefExternal incRefInternal parent
    decRef = maybe decRefExternal decRefInternal parent

swallowOr :: Hashable t => Graph t -> Maybe RefIx -> IS.IntSet -> RefIx -> IO IS.IntSet
swallowOr graph parent uniqXs refix = do
  let rec uniqXs refix@(RefIx x)
        | IS.member x uniqXs = do
          decRef refix graph
          return uniqXs
        | otherwise = do
          getRef refix graph >>= \case
            CountedRef{contents = IOr childXs, parentCount = 1} -> do
              forIntSet_ childXs \i -> incRef (RefIx i) graph
              decRef refix graph
              intSetFoldM uniqXs childXs \i uniqXs' -> rec uniqXs' (RefIx i)
            _ -> return (IS.insert x uniqXs)
  rec uniqXs refix
  where
    incRef = maybe incRefExternal incRefInternal parent
    decRef = maybe decRefExternal decRefInternal parent

swallowNot :: Hashable t => Graph t -> Maybe RefIx -> RefIx -> IO (Maybe RefIx)
swallowNot graph parent refix = do
  let rec refix = do
        getRef refix graph >>= \case
          CountedRef{contents = INot refix'} -> do
            incRef refix' graph
            decRef refix graph
            rec refix' <&> \mrefix'' -> mrefix'' <|> Just refix'
          _ -> return Nothing
  rec refix
  where
    incRef = maybe incRefExternal incRefInternal parent
    decRef = maybe decRefExternal decRefInternal parent

simplifyNew :: Hashable t => Term t RefIx -> Graph t -> IO (Simplification t)
simplifyNew (And xs) graph = do
  xs' <- foldM (swallowAnd graph Nothing) IS.empty xs
  case intSetDumbSize xs' of
    Zero -> return $ Result True
    One -> return $ Singleton (RefIx $ head $ IS.toList xs')
    Many -> return $ Formula (IAnd xs')
simplifyNew (Or xs) graph = do
  xs' <- foldM (swallowOr graph Nothing) IS.empty xs
  return case intSetDumbSize xs' of
    Zero -> Result False
    One -> Singleton (RefIx $ head $ IS.toList xs')
    Many -> Formula (IOr xs')
simplifyNew (Not refix) graph = do
  swallowNot graph Nothing refix <&> \case
    Just refix' -> Singleton refix'
    Nothing -> Formula (INot refix)
simplifyNew (Leaf x) graph = return $ Formula $ ILeaf x
