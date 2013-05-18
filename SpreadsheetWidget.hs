{-# LANGUAGE FlexibleInstances #-}
module SpreadsheetWidget where

import SpreadsheetCommon
import GetPut
import Data.Map (Map)
import Data.Set (Set)
import System.Environment
import System.IO

import Control.Applicative
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Text.Parsec hiding (many, optional, (<|>))

type CellDomain = Set CellName
type DangerZone = Set CellDomain
type Method     = SpreadsheetValues -> SpreadsheetValues
type Invariant  = SpreadsheetValues -> Bool

data Widget = Widget
  { domain       :: CellDomain
  , existentials :: CellDomain -- note; domain should be distinct from existentials
  , invariant    :: Invariant
  , danger       :: DangerZone
  , methods      :: CellDomain -> Method
  }
data Spreadsheet = Spreadsheet
  { widget :: Widget
  , locked :: CellDomain
  , values :: SpreadsheetValues
  }
  
-- Can also add inequalities at a later date
data Relation = Eq Formula Formula | And Relation Relation
  deriving (Eq, Ord, Show, Read)
  
-- Define RelationWidgets in terms of equations and relations we can read,
-- then translate them into methods and invariants
data RelationWidget = RelationWidget
  { relation  :: Relation
  , equations :: Map CellDomain (Set Equation)
  , dangerZone:: DangerZone
  }
  
  
-- default, identity widget which matches with everything
defaultSpreadsheet :: Spreadsheet
defaultSpreadsheet = Spreadsheet { widget = defaultWidget, locked = Set.empty, values = Map.empty }

defaultWidget :: Widget
defaultWidget = Widget { domain = dom, existentials = exs, invariant = inv, danger = dz, methods = m } where
  dom = Set.empty
  exs = Set.empty
  inv = \ _ -> True
  dz  = Set.empty
  m   = \ _ -> id

-----------------------------------------------------------------
------------------- Pretty Printer ------------------------------
  
instance PPrint Relation where
  pprint (Eq f1 f2) = pprint f1 ++ " = " ++ pprint f2
  pprint (And r1 r2) = pprint r1 ++ " /\\ " ++ pprint r2 
  

instance PPrint (Map CellDomain (Set Equation)) where
  pprint m = unlines [pprint dom ++ " : " ++ pprint set_eq | (dom, set_eq) <- Map.assocs m]


instance PPrint RelationWidget where
  pprint w = 
    pprint (relation w) ++ "\n" ++ pprint (dangerZone w) ++ "\n" ++ pprint (equations w)

instance PPrint [Relation] where
  pprint ls = unlines (map pprint ls)
  
instance PPrint [RelationWidget] where  
  pprint ls = intercalate "--\n" (map pprint ls)

instance PPrint Bool where
  pprint True = "True"
  pprint False = "False"
  
instance PPrint Spreadsheet where
  pprint sheet = "\n Locked cells: " ++ slocked ++ "\n Danger Zone: " ++ sdz ++ "\n Values: " ++ "\n" ++ svals where
    sdz     = pprint $ danger $ widget sheet
    slocked = pprint $ locked sheet
    svals   = pprint $ restrictDom (values sheet) (domain $ widget sheet) 

-------------------------------------------------------------------------
----------------------------- Composition -------------------------------
  
restrictDom :: SpreadsheetValues -> CellDomain -> SpreadsheetValues
restrictDom vals dom = -- Map.intersection vals . Map.fromSet (const ())
  Map.filterWithKey (\ s _ -> Set.member s dom) vals

safe, dangerous :: DangerZone -> CellDomain -> Bool
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

composeSheet :: Spreadsheet -> Spreadsheet -> Spreadsheet
composeSheet 
  (Spreadsheet { widget = wk, locked = lk, values = valsk })
  (Spreadsheet { widget = wl, locked = ll, values = valsl })
  = Spreadsheet { widget = w, locked = l, values = vals } where
    w = compose wk wl
    l = Set.union lk ll
    vals = Map.union valsk valsl

compose :: Widget -> Widget -> Widget
compose
  ( Widget { domain = domk, existentials = exsk, invariant = invk, danger = dzk, methods = fk })
  ( Widget { domain = doml, existentials = exsl, invariant = invl, danger = dzl, methods = fl })
  = Widget { domain = dom , existentials = exs , invariant = inv , danger = dz , methods = f  } where
  dom        = Set.union domk doml
  left s     = "l_" ++ s
  right s    = "r_" ++ s
  accName s  = 
    if Set.member s exsk then left s
    else if Set.member s exsl then right s
         else s
  exs        = Set.union (Set.map left exsk) (Set.map right exsl) -- disjoint union
  -- valsk vals[name] = vals[name] if name \in domk
  -- valsk vals[name] = vals[l_name] if name \in exsk 
  valsk vals = Map.union (restrictDom vals domk) evalsk where
    evalsk = Set.fold g Map.empty exsk
    g name = Map.alter (\ _ -> Map.lookup (left name) vals) name
  valsl vals = Map.union (restrictDom vals doml) evalsl where
    evalsl = Set.fold g Map.empty exsl
    g name = Map.alter (\ _ -> Map.lookup (right name) vals) name 
  inv vals = invk (valsk vals) && invl (valsl vals)
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
      vals_runk = fk kIn (valsk vals)
      vals_runl = fl lInShared (valsl vals_runk `Map.union` valsl vals)
      in (Map.mapKeys accName vals_runk) `Map.union` (Map.mapKeys accName vals_runl)
    runLFirst vals = let
      vals_runl = fl lIn (valsl vals)
      vals_runk = fk kInShared (valsk vals_runl `Map.union` valsk vals)
      in (Map.mapKeys accName vals_runl) `Map.union` (Map.mapKeys accName vals_runk)

-------------------------------------------------------------------------
----------------------------- Hide --------------------------------------

hide :: CellName -> Widget -> Widget
hide c w =
  if not (Set.member c $ domain w) 
  then w
  else Widget { domain = dom , existentials = exs , invariant = inv , danger = dz , methods = f  } where
    dom = Set.delete c (domain w)
    exs = Set.insert c (existentials w)
    inv = invariant w
    dz  = Set.filter (\s -> not $ Set.member c s) (danger w)
    f   = methods w

-------------------------------------------------------------------------
--------------------------------- Step ----------------------------------

step :: Widget -> SpreadsheetValues -> SpreadsheetValues -> SpreadsheetValues
step w vals input =
  let i = Map.keysSet input in
  if dangerous (danger w) i || (not $ Set.isSubsetOf i (domain w)) 
  then vals
  else 
    let zeros dom = Set.fold g Map.empty dom where
          g cell vals = Map.insert cell 0 vals
    in (methods w) i (Map.union input vals) `Map.union` zeros (domain w)

stepSheet :: Spreadsheet -> SpreadsheetValues -> Spreadsheet
stepSheet sheet input = 
  let locked_vals = restrictDom (values sheet) (locked sheet) in
  let input' = Map.union locked_vals input in
  let vals = step (widget sheet) (values sheet) input' in
  Spreadsheet { widget = widget sheet, locked = locked sheet, values = vals }

-----------------------------------------------------------------
--------------------- RelationWidget to Widgets -----------------
  
getDomain :: RelationWidget -> CellDomain 
getDomain w = Set.unions $ Map.keys (equations w)

--this invariant doesn't relate hidden values
getInvariant :: Relation -> Invariant
getInvariant (Eq f1 f2) vals = 
  let mv1 = cell_get_maybe f1 vals in
  let mv2 = cell_get_maybe f2 vals in
  Maybe.fromMaybe False $ liftM2 (==) mv1 mv2
  
getMethods :: Map CellDomain (Set Equation) -> CellDomain -> Method
getMethods eqns dom vals = 
  -- instantiate the undefined variables in vals to 0
  let vals' = Map.union vals dom_vals where
        dom_vals = Map.fromList (zip (Set.elems dom) zeros)
        zeros = 0 : zeros
  in
  -- input equations to update domain variables
  let dom_eqns = Maybe.fromMaybe Set.empty $ Map.lookup dom eqns in
  let new_vals = Set.fold g Map.empty dom_eqns where
        g (Equation target f) new = Map.insert target (cell_get f vals') new
  in
   Map.union new_vals vals'


getWidget :: RelationWidget -> Widget
getWidget w = Widget { domain = dom , existentials = exs , invariant = inv , danger = dz , methods = f  } where
  dom = getDomain w 
  exs = Set.empty
  inv = getInvariant $ relation w 
  dz  = dangerZone w
  f   = getMethods $ equations w



-------------------------------------------------------------------------
------------------------------ Parser -----------------------------------
    
instance Parseable RelationWidget where
  parser = do
    rel <- parser -- relation
    string "\n"
    dz <- parser -- danger zone
    string "\n"
    eqns <- parser -- equations
    string "!\n"
    return (RelationWidget { relation = rel, dangerZone = dz, equations = eqns })
    
instance Parseable [Relation] where
  parser = sepBy parser (string ", ")

instance Parseable [RelationWidget] where
  parser = sepBy parser (string "---\n") -- RelationWidget
    
instance Parseable Widget where
  parser = getWidget <$> parser -- RelationWidget
        
instance Parseable Relation where
  parser = do
    f1 <- parser -- formula
    string " = " 
    f2 <- parser -- formula
    return (Eq f1 f2)
  
    
instance Parseable DangerZone where
  parser = Set.fromList <$> sepBy parser (string ", ") -- CellDomain
  
instance Parseable (Map CellDomain (Set Equation)) where
  parser = do
    l <- sepBy parser (string "#\n") -- (CellDomain, Set Equation)
    return (Map.fromList l)
    
instance Parseable (Set Equation) where
  parser = Set.fromList <$> sepBy parser (string ", ") -- parser here: Equation
    
instance Parseable (CellDomain,Set Equation) where
  parser = do
    i <- parser -- CellDomain 
    string ": "
    e <- parser -- Set Equation 
    return (i,e)
    
instance Parseable CellDomain where
  parser = Set.fromList <$> sepBy parseCellName (string " ")
    
prompt s = do
  putStr s
  hFlush stdout
  getLine
  
readsWords = mapM noJunk . words where
  noJunk s = [v | (v, "") <- reads s]
    
setValueLoop w s = do
  putStrLn (pprint s)
  putStrLn ("Satisfies Invariant: " ++ (pprint $ invariant w s) ++ "\n")
  cellNames <- words <$> prompt "Change cells: "
  case cellNames of
    [] -> setValueLoop w (step w s Map.empty)
    _  -> 
      let dom = Set.fromList cellNames in
      if dangerous (danger w) dom || (not $ Set.isSubsetOf dom (domain w))
      then putStrLn "That domain is dangerous!" >> setValueLoop w s
      else do
           cellValues <- prompt "Change values to: "
           case readsWords cellValues of
             [] -> putStrLn "That didn't look like numbers to me!" >> setValueLoop w s
             cellValues:_
               | length cellValues /= length cellNames -> 
                 putStrLn "Please give as many values as you gave names." >> setValueLoop w s
               | otherwise -> setValueLoop w (step w s (Map.fromList (zip cellNames cellValues)))

  
main = do
  a <- getArgs
  case a of
    [fileName] -> do
      s <- readFile fileName
      case parse parser fileName s of
        Left err -> print err
        Right s -> -- set of relation widgets 
          let w = foldl g defaultWidget s where
                g w rw = compose w (getWidget rw)
          in do 
            putStrLn (pprint s)
            setValueLoop w Map.empty
    _ -> putStrLn "Call with one argument naming a file with some relations, danger zones, methods."