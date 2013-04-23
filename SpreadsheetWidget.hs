module SpreadsheetWidget where

import SpreadsheetCommon
import Data.Map (Map)
import Data.Set (Set)

import qualified Data.Set as Set
import qualified Data.Map as Map

type CellDomain = Set CellName
type DangerZone = Set (Set CellName)
type Method     = SpreadsheetValues -> SpreadsheetValues

data Widget = Widget
  { domain    :: CellDomain
  , invariant :: SpreadsheetValues -> Bool
  , danger    :: DangerZone
  , methods   :: CellDomain -> Method
  }

restrictDom :: SpreadsheetValues -> CellDomain -> SpreadsheetValues
restrictDom vals = Map.intersection vals . Map.fromSet (const ())

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
--
-- TODO: in fact, we don't even do the easy rule yet!
minimizeDangerZone x = x

compose :: Widget -> Widget -> Widget
compose
  ( Widget { domain = domk, invariant = invk, danger = dzk, methods = fk })
  ( Widget { domain = doml, invariant = invl, danger = dzl, methods = fl })
  = Widget { domain = dom , invariant = inv , danger = dz , methods = f  } where
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
