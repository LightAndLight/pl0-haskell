module Data.ITree (
  ITree
  , itree
  , getLeaves
  , setLeaf
  , getValue
  , setValue
) where

import           Data.Map (Map)
import qualified Data.Map as M

newtype ITree k a = ITree { getITree :: (a, Map k (ITree k a)) }
  deriving (Eq,Show)

instance Ord k => Functor (ITree k) where
  fmap f tree = ITree (f $ getValue tree,fmap (fmap f) (getLeaves tree))

itree :: a -> Map k (ITree k a) -> ITree k a
itree a m = ITree (a,m)

getValue :: ITree k a -> a
getValue = fst . getITree

setValue :: a -> ITree k a -> ITree k a
setValue a (ITree (_,m)) = ITree (a,m)

getLeaves :: ITree k a -> Map k (ITree k a)
getLeaves = snd . getITree

setLeaf :: Ord k => k -> a -> ITree k a -> ITree k a
setLeaf k v (ITree (a,m)) = ITree (a,M.insert k (ITree (v,M.empty)) m)
