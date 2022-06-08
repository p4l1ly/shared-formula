{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Formula.NAry.Shared where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Function.Apply ((-$))
import Data.Function.Between
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
      (refix, cons') <- getCompose $
        M.alterF -$ term' -$ cons $ \case
          Just refix -> Compose do
            incRefExternal refix graph
            cref <- getRef refix graph
            itermFor_ (contents cref) (`decRefExternal` graph)
            return (refix, Just refix)
          Nothing -> Compose do
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
                    { contents = term'
                    , parentCount = 1
                    , parents = IS.empty
                    }
            setRef refix cref graph
            onCreate refix graph
            return (refix, Just refix)
      writeIORef (tCons graph) cons'
      return $ Right refix

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
  case term of
    INot refix' -> decRefInternal refix refix' graph
    IAnd refixs -> forIntSet_ refixs \refix' -> decRefInternal refix refix' graph
    IOr refixs -> forIntSet_ refixs \refix' -> decRefInternal refix refix' graph
    ILeaf _ -> return ()
  modifyIORef' (freeList graph) (refix :)

data Change t
  = Evaluated Bool
  | Redirected RefIx
  | Changed
  deriving (Show, Eq)

notifyParents :: Hashable t => RefIx -> Change t -> Graph t -> IO ()
notifyParents refix change graph = do
  CountedRef{parents} <- getRef refix graph
  forIntSet_ parents \parentRefix ->
    onChildChange parentRefix refix change graph

changeChildsAnd :: Hashable t => RefIx -> Maybe IS.IntSet -> Graph t -> IO ()
changeChildsAnd refix childs' graph = do
  node@CountedRef{contents = term@(IAnd childs), parents} <- getRef refix graph
  tCons_ <- readIORef (tCons graph)
  let tCons_' = M.delete term tCons_
  case childs' of
    Nothing -> do
      writeIORef (tCons graph) tCons_'
      notifyParents refix (Evaluated False) graph
    Just childs' ->
      case sizeUpTo 2 childs' of
        0 -> do
          writeIORef (tCons graph) tCons_'
          notifyParents refix (Evaluated True) graph
        1 -> do
          writeIORef (tCons graph) tCons_'
          let refix' = RefIx $ head $ IS.toList childs'
          notifyParents refix (Redirected refix') graph
        _ -> do
          let term' = IAnd childs'
          case M.lookup term' tCons_' of
            Just refix' -> do
              writeIORef (tCons graph) tCons_'
              notifyParents refix (Redirected refix') graph
            Nothing -> do
              writeIORef (tCons graph) (M.insert term' refix tCons_')
              setRef refix node{contents = term'} graph
              notifyParents refix Changed graph

changeChildsOr :: Hashable t => RefIx -> Maybe IS.IntSet -> Graph t -> IO ()
changeChildsOr refix childs' graph = do
  node@CountedRef{contents = term@(IOr childs), parents} <- getRef refix graph
  tCons_ <- readIORef (tCons graph)
  let tCons_' = M.delete term tCons_
  case childs' of
    Nothing -> do
      writeIORef (tCons graph) tCons_'
      notifyParents refix (Evaluated True) graph
    Just childs' ->
      case sizeUpTo 2 childs' of
        0 -> do
          writeIORef (tCons graph) tCons_'
          notifyParents refix (Evaluated False) graph
        1 -> do
          writeIORef (tCons graph) tCons_'
          let refix' = RefIx $ head $ IS.toList childs'
          notifyParents refix (Redirected refix') graph
        _ -> do
          let term' = IOr childs'
          case M.lookup term' tCons_' of
            Just refix' -> do
              writeIORef (tCons graph) tCons_'
              notifyParents refix (Redirected refix') graph
            Nothing -> do
              writeIORef (tCons graph) (M.insert term' refix tCons_')
              setRef refix node{contents = term'} graph
              notifyParents refix Changed graph

onChildSingleParent :: Hashable t => RefIx -> Graph t -> IO ()
onChildSingleParent childRefix@(RefIx childIx) graph = do
  child <- getRef childRefix graph
  case contents child of
    IAnd childChilds -> do
      forIntSet_ (parents child) \parentRefix -> do
        parent <- getRef parentRefix graph
        case contents parent of
          old@(IAnd parentChilds) -> do
            -- We will refer the grandchildren.
            forIntSet_ childChilds \i -> incRefInternal parentRefix i graph
            -- The child can be unreferred (and thus deallocated).
            decRefInternal parentRefix childRefix graph
            -- Let's remove the child and add the grandchildren.
            let parentChilds' = IS.delete childIx parentChilds
            parentChilds'' <-
              swallowAnd graph (Just parentRefix) $
                intSetFoldM parentChilds' childChilds
            when (Just parentChilds' /= parentChilds'') do
              changeChildsAnd parentRefix parentChilds'' graph
          _ -> return ()
    IOr childChilds ->
      forIntSet_ (parents child) \parentRefix -> do
        parent <- getRef parentRefix graph
        case contents parent of
          old@(IOr parentChilds) -> do
            -- We will refer the grandchildren.
            forIntSet_ childChilds \i -> incRefInternal parentRefix i graph
            -- The child can be unreferred (and thus deallocated).
            decRefInternal parentRefix childRefix graph
            -- Let's add the grandchildren.
            let parentChilds' = IS.delete childIx parentChilds
            parentChilds'' <-
              swallowOr graph (Just parentRefix) $
                intSetFoldM parentChilds' childChilds
            when (Just parentChilds' /= parentChilds'') do
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
        | b -> changeChildsAnd parentRefix (Just $ IS.delete childIx parentChilds) graph
        | otherwise -> notifyParents parentRefix (Evaluated False) graph
      IOr parentChilds
        | b -> notifyParents parentRefix (Evaluated True) graph
        | otherwise ->
            changeChildsOr parentRefix (Just $ IS.delete childIx parentChilds) graph
      ILeaf t -> error "ILeaf: onChildChange"
  Redirected childRefix' -> do
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
          swallowAnd graph (Just parentRefix) $ parentChilds' ~$~ childRefix'
        changeChildsAnd parentRefix parentChilds'' graph
      IOr parentChilds -> do
        let parentChilds' = IS.delete childIx parentChilds
        parentChilds'' <-
          swallowOr graph (Just parentRefix) $ parentChilds' ~$~ childRefix'
        changeChildsOr parentRefix parentChilds'' graph
      ILeaf t -> error "ILeaf: onChildChange"
  Changed -> do
    parent <- getRef parentRefix graph
    case contents parent of
      IAnd parentChilds -> do
        -- call swallowAnd only to handle absorption (no new children, only grandchildren)
        parentChilds' <- swallowAnd graph (Just parentRefix) $ \_ -> return parentChilds
        when (Just parentChilds /= parentChilds') do
          changeChildsAnd parentRefix parentChilds' graph
      IOr parentChilds -> do
        -- call swallowOr only to handle absorption (no new children, only grandchildren)
        parentChilds' <- swallowOr graph (Just parentRefix) $ \_ -> return parentChilds
        when (Just parentChilds /= parentChilds') do
          changeChildsOr parentRefix parentChilds' graph
      INot _ -> return ()
      ILeaf _ -> error "ILeaf: onChildChange"

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
    modifyRef i -$ graph $ \child@CountedRef{parents} ->
      child{parents = IS.insert parent parents}

forIntSet_ :: Applicative f => IS.IntSet -> (RefIx -> f b) -> f ()
forIntSet_ xs step = IS.fold (\i a -> a <* step (RefIx i)) (pure ()) xs

intSetFoldM :: Monad f => b -> IS.IntSet -> (b -> RefIx -> f b) -> f b
intSetFoldM init xs step = IS.fold (\i a -> a >>= (`step` RefIx i)) (pure init) xs

itermFor_ :: Applicative f => ITerm t -> (RefIx -> f b) -> f ()
itermFor_ (IAnd xs) step = forIntSet_ xs step
itermFor_ (IOr xs) step = forIntSet_ xs step
itermFor_ (INot (RefIx x)) step = () <$ step (RefIx x)
itermFor_ (ILeaf x) step = pure ()

itermFoldM :: Monad f => b -> ITerm t -> (b -> RefIx -> f b) -> f b
itermFoldM b (IAnd xs) step = intSetFoldM b xs step
itermFoldM b (IOr xs) step = intSetFoldM b xs step
itermFoldM b (INot x) step = step b x
itermFoldM b (ILeaf x) step = pure b

sizeUpTo :: Int -> IS.IntSet -> Int
sizeUpTo max xs = IS.fold (\_ fn z -> if z == max then max else fn (z + 1)) id xs 0

swallowAnd ::
  Hashable t =>
  Graph t ->
  Maybe RefIx ->
  ((IS.IntSet -> RefIx -> IO IS.IntSet) -> IO IS.IntSet) ->
  IO (Maybe IS.IntSet)
swallowAnd graph parent extFold = do
  -- Flattening
  let rec uniqXs refix@(RefIx x)
        | IS.member x uniqXs = do
            decRef refix graph
            return uniqXs
        | otherwise = do
            getRef refix graph >>= \case
              CountedRef{contents = IAnd childXs, parentCount = 1} -> do
                forIntSet_ childXs (`incRef` graph)
                decRef refix graph
                intSetFoldM uniqXs childXs rec
              _ -> return (IS.insert x uniqXs)
  myElems <- extFold rec

  -- Absorption
  myElems' <- intSetFoldM myElems myElems \myElems' refix@(RefIx ix) -> do
    getRef refix graph >>= \case
      CountedRef{contents = IOr childXs}
        | IS.fold (\i r -> IS.member i myElems || r) False childXs -> do
            decRef refix graph
            return (IS.delete ix myElems')
      _ -> return myElems'

  -- Negation law: a AND NOT a = 0
  evaluated <- intSetFoldM False myElems' \case
    True -> \_ -> return True
    False -> \refix ->
      getRef refix graph >>= \case
        CountedRef{contents = INot (RefIx nix)} -> return $ IS.member nix myElems'
        -- Deep negation law: a AND (b AND NOT a) = 0
        CountedRef{contents = IAnd childIxs} -> do
          intSetFoldM False childIxs \case
            True -> \_ -> return True
            False -> \refix ->
              getRef refix graph >>= \case
                CountedRef{contents = INot (RefIx nix)} ->
                  return $ IS.member nix myElems'
                _ -> return False
        _ -> return False
  if evaluated
    then return Nothing
    else do
      -- Law of common identities: a OR (b AND NOT a) = a OR b
      intSetFoldM (Just myElems') myElems' \case
        Nothing -> \_ -> return Nothing
        Just myElems'' -> \childRefix@(RefIx childIx) -> do
          getRef childRefix graph >>= \case
            node@CountedRef{contents = IOr childIxs, parentCount = 1} -> do
              removed <- intSetFoldM [] childIxs \removed grandchildRefix -> do
                getRef grandchildRefix graph >>= \case
                  CountedRef{contents = INot (RefIx grandgrandchildIx)}
                    | IS.member grandgrandchildIx myElems'' -> return (grandchildRefix : removed)
                  _ -> return removed
              case removed of
                [] -> return (Just myElems'')
                _ -> do
                  for_ removed $ \grandchildIx ->
                    decRefInternal childRefix grandchildIx graph
                  let removedSet = IS.fromList $ map (\(RefIx i) -> i) removed
                  childIxs' <- swallowOr graph (Just childRefix) $ \_ ->
                    return $ IS.difference childIxs removedSet
                  case childIxs' of
                    Nothing -> do
                      decRef childRefix graph
                      swallowAnd graph parent $ \_ -> return $ IS.delete childIx myElems''
                    Just childIxs' ->
                      case sizeUpTo 2 childIxs' of
                        0 -> do
                          forIntSet_ myElems'' (`decRef` graph)
                          return Nothing
                        1 -> do
                          let theOneRefix = RefIx $ head $ IS.toList childIxs'
                          incRef theOneRefix graph
                          decRef childRefix graph
                          swallowAnd graph parent $ IS.delete childIx myElems'' ~$~ theOneRefix
                        _ -> do
                          tCons_ <- readIORef (tCons graph)
                          let tCons_' = M.delete (IOr childIxs) tCons_
                          let term' = IOr childIxs'
                          case M.lookup term' tCons_' of
                            Just childRefix' -> do
                              incRef childRefix' graph
                              decRef childRefix graph
                              writeIORef (tCons graph) tCons_'
                              swallowAnd graph parent $ IS.delete childIx myElems'' ~$~ childRefix'
                            Nothing -> do
                              writeIORef (tCons graph) (M.insert term' childRefix tCons_')
                              setRef childRefix node{contents = term'} graph
                              swallowAnd graph parent $ \_ -> return myElems''
            _ -> return (Just myElems'')
  where
    incRef = maybe incRefExternal incRefInternal parent
    decRef = maybe decRefExternal decRefInternal parent

swallowOr ::
  Hashable t =>
  Graph t ->
  Maybe RefIx ->
  ((IS.IntSet -> RefIx -> IO IS.IntSet) -> IO IS.IntSet) ->
  IO (Maybe IS.IntSet)
swallowOr graph parent extFold = do
  -- Flattening
  let rec uniqXs refix@(RefIx x)
        | IS.member x uniqXs = do
            decRef refix graph
            return uniqXs
        | otherwise = do
            getRef refix graph >>= \case
              CountedRef{contents = IOr childXs, parentCount = 1} -> do
                forIntSet_ childXs (`incRef` graph)
                decRef refix graph
                intSetFoldM uniqXs childXs rec
              _ -> return (IS.insert x uniqXs)
  myElems <- extFold rec

  -- Absorption
  myElems' <- intSetFoldM myElems myElems \myElems' childRefix@(RefIx childIx) -> do
    getRef childRefix graph >>= \case
      CountedRef{contents = IAnd childXs}
        | IS.fold (\i r -> IS.member i myElems || r) False childXs -> do
            decRef childRefix graph
            return (IS.delete childIx myElems')
      _ -> return myElems'

  -- Negation law: a OR NOT a = 1
  evaluated <- intSetFoldM False myElems' \case
    True -> \_ -> return True
    False -> \childRefix ->
      getRef childRefix graph >>= \case
        CountedRef{contents = INot (RefIx nix)} -> return $ IS.member nix myElems'
        -- Deep negation law: a OR (b OR NOT a) = 1
        CountedRef{contents = IOr childIxs} -> do
          intSetFoldM False childIxs \case
            True -> \_ -> return True
            False -> \childRefix ->
              getRef childRefix graph >>= \case
                CountedRef{contents = INot (RefIx nix)} ->
                  return $ IS.member nix myElems'
                _ -> return False
        _ -> return False
  if evaluated
    then return Nothing
    else do
      -- Law of common identities: a OR (b AND NOT a) = a OR b
      intSetFoldM (Just myElems') myElems' \case
        Nothing -> \_ -> return Nothing
        Just myElems'' -> \childRefix@(RefIx childIx) -> do
          getRef childRefix graph >>= \case
            node@CountedRef{contents = IAnd childIxs, parentCount = 1} -> do
              removed <- intSetFoldM [] childIxs \removed grandchildRefix -> do
                getRef grandchildRefix graph >>= \case
                  CountedRef{contents = INot (RefIx grandgrandchildIx)}
                    | IS.member grandgrandchildIx myElems'' -> return (grandchildRefix : removed)
                  _ -> return removed
              case removed of
                [] -> return (Just myElems'')
                _ -> do
                  for_ removed $ \grandchildIx ->
                    decRefInternal childRefix grandchildIx graph
                  let removedSet = IS.fromList $ map (\(RefIx i) -> i) removed
                  childIxs' <- swallowAnd graph (Just childRefix) $ \_ ->
                    return $ IS.difference childIxs removedSet
                  case childIxs' of
                    Nothing -> do
                      decRef childRefix graph
                      swallowOr graph parent $ \_ -> return $ IS.delete childIx myElems''
                    Just childIxs' ->
                      case sizeUpTo 2 childIxs' of
                        0 -> do
                          forIntSet_ myElems'' (`decRef` graph)
                          return Nothing
                        1 -> do
                          let theOneRefix = RefIx $ head $ IS.toList childIxs'
                          incRef theOneRefix graph
                          decRef childRefix graph
                          swallowOr graph parent $ IS.delete childIx myElems'' ~$~ theOneRefix
                        _ -> do
                          tCons_ <- readIORef (tCons graph)
                          let tCons_' = M.delete (IAnd childIxs) tCons_
                          let term' = IAnd childIxs'
                          case M.lookup term' tCons_' of
                            Just childRefix' -> do
                              incRef childRefix' graph
                              decRef childRefix graph
                              writeIORef (tCons graph) tCons_'
                              swallowOr graph parent $ IS.delete childIx myElems'' ~$~ childRefix'
                            Nothing -> do
                              writeIORef (tCons graph) (M.insert term' childRefix tCons_')
                              setRef childRefix node{contents = term'} graph
                              swallowOr graph parent $ \_ -> return myElems''
            _ -> return (Just myElems'')
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
  xs' <- swallowAnd graph Nothing $ foldM -$ IS.empty -$ xs
  case xs' of
    Nothing -> return $ Result False
    Just xs' ->
      case sizeUpTo 2 xs' of
        0 -> return $ Result True
        1 -> return $ Singleton (RefIx $ head $ IS.toList xs')
        _ -> return $ Formula (IAnd xs')
simplifyNew (Or xs) graph = do
  xs' <- swallowOr graph Nothing $ foldM -$ IS.empty -$ xs
  case xs' of
    Nothing -> return $ Result True
    Just xs' ->
      return case sizeUpTo 2 xs' of
        0 -> Result False
        1 -> Singleton (RefIx $ head $ IS.toList xs')
        _ -> Formula (IOr xs')
simplifyNew (Not refix) graph = do
  swallowNot graph Nothing refix <&> \case
    Just refix' -> Singleton refix'
    Nothing -> Formula (INot refix)
simplifyNew (Leaf x) graph = return $ Formula $ ILeaf x
