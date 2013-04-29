module SpreadsheetWidget where

import SpreadsheetCommon
import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe

type CellDomain = Set CellName
type DangerZone = Set CellDomain
type Method     = SpreadsheetValues -> SpreadsheetValues

data GenWidget = GenWidget
  { domain    :: CellDomain
  , invariant :: SpreadsheetValues -> Bool
  , danger    :: DangerZone
  , methods   :: CellDomain -> Method
  }
  
-- Can also add inequalities at a later date
data Relation = Eq Formula Formula | Conjunction Conj Relation Relation
  deriving (Eq, Ord, Show, Read)
data Conj = And | Or
  deriving (Eq, Ord, Show, Read)
  
-- Define widgets in terms of equations and relations we can read,
-- then translate them into methods and invariants
data Widget = Widget
  { relation  :: Relation
  , equations :: Map CellDomain (Set Equation)
  }
  
getDomain :: Widget -> CellDomain 
getDomain w = Set.unions $ Map.keys (equations w)

getInvariant :: Relation -> SpreadsheetValues -> Bool
getInvariant (Eq f1 f2) vals = 
  let mv1 = cell_get_maybe f1 vals in
  let mv2 = cell_get_maybe f2 vals in
  Maybe.fromMaybe False $ liftM2 (==) mv1 mv2
  
cell_get_maybe :: Formula -> SpreadsheetValues -> Maybe Value
cell_get_maybe (Cell new_name) values = Map.lookup new_name values
cell_get_maybe (BinOp o f1 f2) values = liftM2 (op o) (cell_get_maybe f1 values) (cell_get_maybe f2 values)
  
-----------------------------------------------------------------
------------------- Pretty Printer ------------------------------
  
instance PPrint Relation where
  pprint (Eq f1 f2) = pprint f1 ++ " = " ++ pprint f2
  pprint (Conjunction And r1 r2) = pprint r1 ++ " /\\ " ++ pprint r2 
  pprint (Conjunction Or  r1 r2) = pprint r1 ++ " \\/ " ++ pprint r2
  
--instance PPrint (Map CellDomain (Set Equation)) where
--  pprint m = unlines [ pprint dom ++ " : " ++ pprint eq_set | (dom, eq_set) <- Map.assocs m]
  

-------------------------------------------------------------------------
----------------------------- Composition -------------------------------
  
restrictDom :: SpreadsheetValues -> CellDomain -> SpreadsheetValues
restrictDom vals dom = -- Map.intersection vals . Map.fromSet (const ())
  Map.intersection vals (Map.fromList $ List.map (\ x -> (x,1)) (Set.elems dom))

safe, dangerous :: DangerZone -> Set CellName -> Bool
safe = (not .) . dangerous
dangerous rep s = any (`Set.isSubsetOf` s) (Set.toList rep)

-- Minimize the representation of a danger zone.
-- Given a representation rep, this function attempts to produce a smaller
-- representation rep' such that dangerous rep = dangerous rep'. At the moment,
-- it does this only by iterating one rule: remove any sets of names which are
-- proper supersets of another set of names in the representation. To be
-- complete, we would probably also need a rule that said something like "If
-- all of the sets of names S U {x} for x in the domain of the lens are in the
-- representation, replace them with the single set S.". We don't try to do
-- that yet.
minimizeDangerZone x = Set.filter (\s -> and [not (Set.isProperSubsetOf s' s) | s' <- Set.toList x]) x

compose :: GenWidget -> GenWidget -> GenWidget
compose
  ( GenWidget { domain = domk, invariant = invk, danger = dzk, methods = fk })
  ( GenWidget { domain = doml, invariant = invl, danger = dzl, methods = fl })
  = GenWidget { domain = dom , invariant = inv , danger = dz , methods = f  } where
  dom      = Set.union domk doml
  inv vals = invk (restrictDom vals domk) && invl (restrictDom vals doml)
  shared   = domk `Set.intersection` doml
  dz       = minimizeDangerZone (dzk `Set.union` dzl `Set.union` Set.fromList
                                   [ (vk `Set.union` vl) `Set.difference` shared
                                   | vk <- Set.toList dzk
                                   , vl <- Set.toList dzl
                                   ])
  f inputs = if canRunKFirst then runKFirst else runLFirst where
    kIn = Set.intersection inputs domk
    lIn = Set.intersection inputs doml
    kInShared = Set.union kIn shared
    lInShared = Set.union lIn shared

    canRunKFirst   = safe dzk kIn && safe dzl lInShared
    runKFirst vals = let
      vals'  = fk kIn (restrictDom vals domk)
      vals'' = fl lInShared (restrictDom vals' doml `Map.union` restrictDom vals doml)
      in vals' `Map.union` vals''
    runLFirst vals = let
      vals'  = fl lIn (restrictDom vals doml)
      vals'' = fk kInShared (restrictDom vals' domk `Map.union` restrictDom vals domk)
      in vals' `Map.union` vals''

