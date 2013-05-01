module Spreadsheet where

import SpreadsheetCommon
import System.Environment
import System.IO
import GetPut

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Debug.Trace as Trace


data Spreadsheet = Spreadsheet
  { formulas     :: SpreadsheetFormulas
  , values       :: SpreadsheetValues
    -- dependencies maps cell v to a list of its parents [v_1,...,v_n] 
    -- such that formula v1 is a function of v
  , dependencies :: DependencyGraph
  } deriving (Eq, Ord, Show, Read)
             
null_sheet = Spreadsheet Map.empty Map.empty Map.empty

updateValues :: SpreadsheetValues -> Spreadsheet -> Spreadsheet
updateValues new_values sheet = Spreadsheet (formulas sheet) new_values (dependencies sheet)
updateValue :: CellName -> Value -> Spreadsheet -> Spreadsheet
updateValue name val sheet = updateValues (Map.insert name val (values sheet)) sheet


-----------------------------------------------------------------
------------------- Pretty Printer ------------------------------

instance PPrint Spreadsheet where
  pprint sheet = unlines $ ["Formulas", pprint (formulas sheet), "Values", pprint (values sheet)]
  
----------------------------------------------------------------
---------------------- Parser ----------------------------------
  
instance Parseable Spreadsheet where 
  parser = (\ s -> Maybe.fromMaybe null_sheet (finalize s)) <$> parser



--------------------------------------------------------------------
--------------------- Evaluation -----------------------------------

-- 1 -- add all formulas
addFormula :: SpreadsheetFormulas -> CellName -> Formula -> SpreadsheetFormulas
addFormula formulas name formula = Map.insert name formula formulas

-- 2 -- build a dependency graph from the formulas given, and check
--      that the graph forms a tree
finalize :: SpreadsheetFormulas -> Maybe Spreadsheet
build_dependencies :: SpreadsheetFormulas -> Set CellName -> DependencyGraph
get_children :: Formula -> Set CellName
get_descendants :: SpreadsheetFormulas -> Set CellName -> Set CellName
get_parents :: DependencyGraph -> Set CellName -> Set CellName
add_parents :: DependencyGraph -> Set CellName -> Set CellName -> DependencyGraph


-- Builds a DependencyGraph where every cell points to the set of its parents.
-- (A parent of v is w with w -> v)
build_dependencies formulas names = Set.fold f Map.empty names where
  f name gr        = add_parents gr (children_of name) (Set.singleton name)
  children_of name = maybe Set.empty get_children (Map.lookup name formulas)

get_children (Cell name)     = Set.singleton name
get_children (BinOp _ f1 f2) = Set.union (get_children f1) (get_children f2)

get_descendants f names = 
  let children = children_of names where
        children_of names = Set.fold g Set.empty names where
          g name children = Set.union children (Maybe.maybe Set.empty get_children (Map.lookup name f)) in
  if Set.null children then names else
    Set.union names (get_descendants f children)
    
get_parents gr cousins = Set.fold f Set.empty cousins where
  get_parent name = Map.findWithDefault Set.empty name gr
  f name uncles = Set.union uncles (get_parent name)

add_parents gr children parents = Set.fold f gr children where
  f child gr = Map.insertWith Set.union child parents gr


  
get_independent_children :: Formula -> Maybe (Set CellName)
get_independent_children (Cell name) = Just $ Set.singleton name
get_independent_children (BinOp _ f1 f2) = 
  let children1 = get_independent_children f1 in
  let children2 = get_independent_children f2 in
  let intersect_is_null s1 s2 = Set.null (Set.intersection s1 s2) in
    if Maybe.fromMaybe False (liftM2 intersect_is_null children1 children2) then 
      liftM2 Set.union children1 children2 
    else
      Nothing

has_independent_children :: SpreadsheetFormulas -> CellName -> Bool
has_independent_children formulas name =
  Maybe.maybe True f (Map.lookup name formulas) where 
    f formula = Maybe.isJust (get_independent_children formula)

  
-- check_dependency should be called on the set of roots
check_single_path :: SpreadsheetFormulas -> DependencyGraph -> CellName -> Set CellName -> Maybe (Set CellName) 
check_single_path formulas gr name seen =
  if Set.member name seen then Nothing 
  else if not (has_independent_children formulas name) then Nothing else 
    Set.fold f (Just (Set.insert name seen)) $ get_parents gr (Set.singleton name) where 
      -- f :: CellName -> Maybe (Set CellName) -> Maybe (Set CellName)
      f parent maybe_seen = Maybe.maybe Nothing (check_single_path formulas gr parent) maybe_seen


-- get nodes which do not have any children
get_roots :: SpreadsheetFormulas -> Set CellName -> Set CellName
get_roots f cells =
  Set.fold g Set.empty cells where
    g name currRoots = Maybe.maybe (Set.insert name currRoots) (\ x -> currRoots) (Map.lookup name f)

check_dependency :: SpreadsheetFormulas -> DependencyGraph -> Bool
check_dependency f gr =
  let roots = get_roots f (Map.keysSet gr) in
  Set.fold g True roots where
    g name False = False
    g name True  = Maybe.isJust (check_single_path f gr name Set.empty) 


finalize formulas = 
  let gr = build_dependencies formulas (Map.keysSet formulas) in
  let roots = get_roots formulas (Map.keysSet formulas) in
  if check_dependency formulas gr then Just (Spreadsheet formulas Map.empty gr) else Nothing
  

--3: edit values in the spreadsheet to populate the spreadsheet
step :: Spreadsheet -> CellName -> Value -> Spreadsheet
step sheet name val =
  let f = formulas sheet in
  let v = values sheet in
  let t = dependencies sheet in
  -- add value to the spreadsheet's values
  let v' = Map.insert name val v in
  -- push down then push up
  let restrict = sweep_put f v' (Set.singleton name) in
  let family = Set.insert name (get_descendants f (Set.singleton name)) in
  let expand = sweep_get f restrict t family in
  updateValues expand sheet

-- check: if the operation doesn't do anything, do not recurse up (or down)
sweep_get :: SpreadsheetFormulas -> SpreadsheetValues -> DependencyGraph -> Set CellName -> SpreadsheetValues
sweep_get f v t siblings = 
  let parents = Set.difference (get_parents t siblings) siblings in
  -- call get on all the parents
  let v' = Set.fold g v parents where
        g parent v' = Maybe.maybe (Map.delete parent v') (\value->Map.insert parent value v') maybe_val where
                      maybe_val = Maybe.maybe Nothing (\formula -> cell_get_maybe formula v') (Map.lookup parent f)
  in 
  if Set.null parents then v' else
    sweep_get f v' t parents
  
--Update the values of your children
sweep_put :: SpreadsheetFormulas -> SpreadsheetValues -> Set CellName -> SpreadsheetValues 
sweep_put f v siblings = Set.fold push v siblings where
  push name v = case Map.lookup name f of
    Just formula -> 
      let value = Map.lookup name v in
      let v' = cell_put formula value v in
      let children = get_children formula in
      sweep_put f v' children
    Nothing      -> v

    
prompt s = do
  putStr s
  hFlush stdout
  getLine
    
setValueLoop s = do
  putStr (pprint s)
  cellName <- prompt "Change cell: "
  cellValue <- prompt "Change value to: "
  case reads cellValue of
    [(v,"")] -> setValueLoop (step s cellName v)
    _ -> putStrLn "That doesn't look like a number to me!" >> setValueLoop s
  
main = do
  a <- getArgs
  case a of
    [fileName] -> do
      s <- readFile fileName
      case parse parser fileName s of
        Left err -> print err
        Right eqs -> setValueLoop eqs
    _ -> putStrLn "Call with one argument naming a file with a list of equations, one on each line"