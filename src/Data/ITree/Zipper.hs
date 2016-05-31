module Data.ITree.Zipper (
  ITreeZipper
  , zipITree
  , up
  , down
  , isTop
  , update
  , extractValue
  , unzipITree
) where

import           Data.ITree

import           Data.Map   (Map)
import qualified Data.Map   as M

newtype ITreeZipper k a = ITreeZipper (ITree k a, [(k,a,Map k (ITree k a))])
  deriving (Eq,Show)

instance Ord k => Functor (ITreeZipper k) where
  fmap f (ITreeZipper (tree,list)) = ITreeZipper (fmap f tree,fmap mapFunction list)
    where
      mapFunction (k,a,trees) = (k,f a,fmap (fmap f) trees)

zipITree :: Ord k => ITree k a -> ITreeZipper k a
zipITree tree = ITreeZipper (tree,[])

up :: Ord k => ITreeZipper k a -> ITreeZipper k a
up z@(ITreeZipper (_,[])) = z
up (ITreeZipper (tree,(key,content,leaves):ts)) = ITreeZipper (itree content $ M.insert key tree leaves,ts)

down :: Ord k => k -> ITreeZipper k a -> ITreeZipper k a
down key z@(ITreeZipper (tree,ts)) = case M.lookup key leaves of
  Just tree' -> ITreeZipper (tree',(key,getValue tree,leaves):ts)
  Nothing -> z
  where
    leaves = getLeaves tree

isTop :: Ord k => ITreeZipper k a -> Bool
isTop (ITreeZipper (_,[])) = True
isTop _ = False

update :: Ord k => (ITree k a -> ITree k a) -> ITreeZipper k a -> ITreeZipper k a
update f (ITreeZipper (t,ts)) = ITreeZipper (f t,ts)

unzipITree :: Ord k => ITreeZipper k a -> ITree k a
unzipITree (ITreeZipper (t,_)) = t

extractValue :: Ord k => ITreeZipper k a -> a
extractValue = getValue . unzipITree
