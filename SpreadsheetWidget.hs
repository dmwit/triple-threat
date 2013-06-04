{-# LANGUAGE FlexibleInstances, TupleSections #-}
module SpreadsheetWidget where

import GetPut
import SpreadsheetCommon
import UpwardClosedSet (UpwardClosedSet)

import Control.Applicative
import Data.List.Split
import Data.Map (Map)
import Data.Set (Set)
import Debug.Trace
import System.Environment
import System.IO
import Text.Parsec hiding (many, optional, (<|>))

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified UpwardClosedSet as Rep

type CellDomain = Set CellName
type DangerZone = UpwardClosedSet CellName
type Method     = SpreadsheetValues -> [SpreadsheetValues]
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
  
-----------------------------------------------------------------
------------------- Default Widget ------------------------------  

-- default, identity widget which matches with everything
defaultSpreadsheet :: Spreadsheet
defaultSpreadsheet = Spreadsheet { widget = defaultWidget, locked = Set.empty, values = Map.empty }

defaultWidget :: Widget
defaultWidget = Widget { domain = dom, existentials = exs, invariant = inv, danger = dz, methods = m } where
  dom = Set.empty
  exs = Set.empty
  inv = \ _ -> True
  dz  = Set.empty
  m   = \ _ -> return

-----------------------------------------------------------------
------------------- Pretty Printer ------------------------------
  
instance PPrint Relation where
  pprint (Eq f1 f2) = pprint f1 ++ " = " ++ pprint f2
  pprint (And r1 r2) = pprint r1 ++ " /\\ " ++ pprint r2 
  

instance PPrint (Map CellDomain (Set Equation)) where
  pprint m = unlines [pprint dom ++ " : " ++ pprint set_eq | (dom, set_eq) <- Map.assocs m]

instance PPrint [Relation] where
  pprint ls = unlines (map pprint ls)
  
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

composeAll :: [Widget] -> Widget
composeAll = composeAll_ . renameAll

renameAll :: [Widget] -> [Widget]
renameAll ws = zipWith rename nameMaps ws where
  domains     = map (Set.toList . domain) ws
  domainNames = Set.unions (map domain ws)
  allNames    = [1..] >>= flip replicateM ['a'..'z']
  freshNames  = allNames \\ Set.toList domainNames
  nameMaps    = zipWith zip domains (splitPlacesBlanks (map length domains) freshNames)

composeAll_ :: [Widget] -> Widget
composeAll_ ws = Widget
  { domain       = Set.unions (map domain ws)
  , existentials = Set.unions (map existentials ws)
  , invariant    = \v -> all (\w -> invariant w (restrictDom v (domain w))) ws
  , danger       = Rep.intersections
    [ Rep.unions
       [ Rep.assume (Set.unions (map domain b)) (danger e)
       | (b, e:_) <- zip (inits order) (tails order)
       ] -- TODO: can probably be more efficient with a scanl, if that matters
    | order <- permutations ws
    ]
  , methods      = \d v -> do
    order <- permutations ws
    guard (canRun d order)
    snd <$> foldM step (d, v) order
  } where
  enlarge d w = Set.union d (domain w)
  canRun d ws = and $ zipWith Rep.notMember (scanl enlarge d ws) (map danger ws)
  step (d, v) w = (enlarge d w,) <$> methods w d v

compose :: Widget -> Widget -> Widget
compose w1 w2 = composeAll [w1, w2]

composeSheet :: Spreadsheet -> Spreadsheet -> Spreadsheet
composeSheet 
  (Spreadsheet { widget = wk, locked = lk, values = valsk })
  (Spreadsheet { widget = wl, locked = ll, values = valsl })
  = errorMessage Spreadsheet { widget = w, locked = l, values = deterministicVals} where
    w = compose wk wl
    l = Set.union lk ll
    vals = Map.union valsk valsl
    nondeterministicVals = step w vals Map.empty
    deterministicVals = case nondeterministicVals of
      [] -> error "running put in composeSheet resulted in no outputs!"
      (x:_) -> x
    errorMessage = case nondeterministicVals of
      [] -> error "running put in composeSheet resulted in no outputs!"
      choices@(x:_:_) -> trace $ "running put in composeSheet resulted in multiple outputs:\n" ++ unlines (map pprint choices) ++ "\nArbitrarily choosing\n" ++ pprint x
      _ -> id

{-
-------------------------------------------------------------------------
----------------------------- Hide --------------------------------------

hide :: CellName -> Widget -> Widget
hide c w =
  if not (Set.member c $ domain w) 
  then w
  else w { domain = Set.delete c (domain w)
         , existentials = Set.insert c (existentials w)
         , danger = Set.filter (\s -> not $ Set.member c s) (danger w)
         }
-}
-------------------------------------------------------------------------
-------------------------------- Rename ---------------------------------

-- rename n1 n2 w = w' where n1 is a cell name is w
renameCell :: CellName -> CellName -> Widget -> Widget
renameCell n1 n2 w = 
  if not (Set.member n1 $ domain w) then w
  else w {domain = dom, invariant = inv, danger = dz, methods = f} where
    changeDom d = Set.insert n2 (Set.delete n1 d)
    changeVals n1 n2 vals = 
      Map.delete n1 $ Map.alter (\ _ -> Map.lookup n1 vals) n2 vals 
    dom      = changeDom (domain w)
    inv vals = invariant w (changeVals n2 n1 vals)
    dz       = Set.map changeDom (danger w)
    f i vals = changeVals n1 n2 <$> (methods w) i (changeVals n2 n1 vals)
  
renameExistential :: CellName -> CellName -> Widget -> Widget
renameExistential n1 n2 w =
  if not (Set.member n1 $ existentials w) then w 
  else w {existentials = exs, invariant = inv, methods = f} where
    change d = Set.insert n2 (Set.delete n1 d)
    changeVals n1 n2 vals =
      Map.delete n1 $ Map.alter (\_ -> Map.lookup n1 vals) n2 vals
    exs      = change (existentials w)
    inv vals = invariant w (changeVals n2 n1 vals)
    f i vals = changeVals n1 n2 <$> (methods w) i (changeVals n2 n1 vals)

rename :: [(CellName,CellName)] -> Widget -> Widget
rename ls w   = foldr g w ls where
  g (n1,n2) w = if Set.member n1 (domain w) 
                then renameCell n1 n2 w
                else if Set.member n1 (existentials w)
                     then renameExistential n1 n2 w
                     else w
-------------------------------------------------------------------------
--------------------------------- Step ----------------------------------

step :: Widget -> SpreadsheetValues -> SpreadsheetValues -> [SpreadsheetValues]
step w vals input =
  let i = Map.keysSet input in
  if Rep.member i (danger w) || (not $ Set.isSubsetOf i (domain w)) 
  then [vals]
  else methods w i (Map.union input vals)

{-
stepSheet :: Spreadsheet -> SpreadsheetValues -> Spreadsheet
stepSheet sheet input = 
  let locked_vals = restrictDom (values sheet) (locked sheet) in
  let input' = Map.union locked_vals input in
  let vals = step (widget sheet) (values sheet) input' in
  sheet {values = vals}

-----------------------------------------------------------------
--------------------- RelationWidget to Widgets -----------------
  
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



-------------------------------------------------------------------------
------------------------------ Parser -----------------------------------

    
instance Parseable [Relation] where
  parser = sepBy parser (string ",\n")

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
  putStrLn ("Satisfies Invariant: " ++ (show $ invariant w s) ++ "\n")
  cellNames <- words <$> prompt "Change cells: "
  case cellNames of
    [] -> setValueLoop w (step w s Map.empty)
    _  -> 
      let dom = Set.fromList cellNames in
      if Rep.member dom (danger w) || (not $ Set.isSubsetOf dom (domain w))
      then putStrLn "That domain is dangerous!" >> setValueLoop w s
      else do
           cellValues <- prompt "Change values to: "
           case readsWords cellValues of
             [] -> putStrLn "That didn't look like numbers to me!" >> setValueLoop w s
             cellValues:_
               | length cellValues /= length cellNames -> 
                 putStrLn "Please give as many values as you gave names." >> setValueLoop w s
               | otherwise -> setValueLoop w (step w s (Map.fromList (zip cellNames cellValues)))

  
-}
