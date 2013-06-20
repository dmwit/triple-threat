module Relation where

import Control.Applicative
import Data.Default
import Data.Set as Set
import FiniteFunction as FF

data Relation a b = Relation
	{ to  :: FiniteFunction a (Set b)
	, fro :: FiniteFunction b (Set a)
	} deriving (Eq, Ord, Show, Read)

instance Default (Relation a b) where def = Relation def def

fromList :: (Ord a, Ord b) => [(a, b)] -> Relation a b
relate   :: (Ord a, Ord b) => a -> b -> Relation a b -> Relation a b
union    :: (Ord a, Ord b) => Relation a b -> Relation a b -> Relation a b
deleteL  :: (Ord a, Ord b) => a -> Relation a b -> Relation a b
deleteR  :: (Ord a, Ord b) => b -> Relation a b -> Relation a b
projectL :: Ord a => a -> Relation a b -> Set b
projectR :: Ord b => b -> Relation a b -> Set a
domain   :: Relation a b -> Set a
codomain :: Relation a b -> Set b
bimapMonotonic :: (a -> a) -> (b -> b) -> Relation a b -> Relation a b

fromList = Prelude.foldr (uncurry relate) def
relate   a b (Relation to fro) = Relation (modify a (insert b) to) (modify b (insert a) fro)
deleteL  a   (Relation to fro) = Relation (set a def to) (Set.foldr (\b -> modify b (delete a)) fro (apply to a))
deleteR    b (Relation to fro) = Relation (Set.foldr (\a -> modify a (delete b)) to (apply fro b)) (set b def fro)
projectL a   rel = apply (to  rel) a
projectR   b rel = apply (fro rel) b
domain   (Relation to fro) = FF.domain to
codomain (Relation to fro) = FF.domain fro
union (Relation to1 fro1) (Relation to2 fro2) = Relation (liftA2 Set.union to1 to2) (liftA2 Set.union fro1 fro2)
bimapMonotonic fa fb (Relation to fro) = Relation (FF.bimapMonotonic fa (Set.mapMonotonic fb) to) (FF.bimapMonotonic fb (Set.mapMonotonic fa) fro)
