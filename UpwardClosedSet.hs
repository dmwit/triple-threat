-- a module for representations of upwards-closed sets; operations are named as
-- if you are operating on the sets

{-# LANGUAGE NoMonomorphismRestriction #-}

module UpwardClosedSet
  ( UpwardClosedSet
  , member, notMember
  , union, intersection, assume
  , unions, intersections
  , minimize
  ) where

import Control.Applicative
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Traversable as T

type UpwardClosedSet a = Set (Set a)

notMember, member :: Ord a => Set a -> UpwardClosedSet a -> Bool
notMember = (not .) . member
member s rep = any (`Set.isSubsetOf` s) (Set.toList rep)

assume        = (minimize .) . assume_
union         = (minimize .) . union_
intersection  = (minimize .) . intersection_
unions        =  minimize    . unions_
intersections =  minimize    . intersections_

union_  = Set.union
unions_ = Set.unions

intersection_  a b  = intersections_ [a, b]
intersections_ reps = Set.fromList $ Set.unions <$> mapM Set.toList reps

-- When talking about danger zones: assume the variables in p have already been
-- modified, so we must make them dangerous to the touch.
assume_ p = Set.map (`Set.difference` p)

-- Minimize a representation.
-- Given a representation rep, this function attempts to produce a smaller
-- representation rep' such that member rep = member rep'. At the moment,
-- it does this only by applying one rule: remove any sets of names which are
-- proper supersets of another set of names in the representation. To be
-- complete, we would probably also need a rule that said something like "If
-- all of the sets of names S U {x} for x in the domain of the lens are in the
-- representation, replace them with the single set S.". We don't try to do
-- that yet.
minimize x = Set.filter (\s -> and [not (Set.isProperSubsetOf s' s) | s' <- Set.toList x]) x
