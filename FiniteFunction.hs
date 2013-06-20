module FiniteFunction where

import Control.Applicative
import Data.Default
import Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data FiniteFunction k v = FF
	{ support     :: Map k v
	, unsupported :: v
	} deriving (Eq, Ord, Show, Read)

instance Default v => Default (FiniteFunction k v) where def = FF def def
instance Functor (FiniteFunction k) where fmap f (FF m v) = FF (fmap f m) (f v)
instance Ord k => Applicative (FiniteFunction k) where
	pure v = FF def v
	FF mf vf <*> FF ma va = FF (mergeWithKey combine only1 only2 mf ma) (vf va) where
		combine k f a = Just (f a)
		only1 = fmap ($va)
		only2 = fmap (vf$)

apply     :: Ord k => FiniteFunction k v -> k -> v
set_      :: Ord k => k -> v -> FiniteFunction k v -> FiniteFunction k v
modify_   :: Ord k => k -> (v -> v) -> FiniteFunction k v -> FiniteFunction k v
tabulate_ :: Ord k => [k] -> v -> (k -> v) -> FiniteFunction k v
set       :: (Ord k, Eq v) => k -> v -> FiniteFunction k v -> FiniteFunction k v
modify    :: (Ord k, Eq v) => k -> (v -> v) -> FiniteFunction k v -> FiniteFunction k v
tabulate  :: (Ord k, Eq v) => [k] -> v -> (k -> v) -> FiniteFunction k v
domain    :: FiniteFunction k v -> Set k
codomain  :: Ord v => FiniteFunction k v -> Set v
minimize  :: (Ord k, Eq v) => FiniteFunction k v -> FiniteFunction k v
bimapMonotonic :: (k -> k) -> (v -> v) -> FiniteFunction k v -> FiniteFunction k v

apply (FF m v) k = findWithDefault v k m
set_ k v (FF m v') = FF (insert k v m) v'
modify_ k f ff = set_ k (f (apply ff k)) ff
tabulate_ ks v f = FF (fromList [(k, f k) | k <- ks]) v
set k v (FF m v') = FF m' v' where
	m' | v == v'   = delete k m
	   | otherwise = insert k v m
modify k f ff = set k (f (apply ff k)) ff
tabulate ks v f = FF (fromList [(k, v') | k <- ks, let v' = f k, v' /= v]) v
domain   (FF m v) = Map.keysSet m
codomain (FF m v) = Set.fromList (v:elems m)
minimize (FF m v) = FF (snd (partition (v==) m)) v
bimapMonotonic fk fv (FF m v) = FF (fv <$> mapKeysMonotonic fk m) (fv v)
