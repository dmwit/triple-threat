module Spreadsheet where

import SpreadsheetCommon
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

updateValues :: SpreadsheetValues -> Spreadsheet -> Spreadsheet
updateValues new_values sheet = Spreadsheet (formulas sheet) new_values (dependencies sheet)
updateValue :: CellName -> Value -> Spreadsheet -> Spreadsheet
updateValue name val sheet = updateValues (Map.insert name val (values sheet)) sheet


-----------------------------------------------------------------
------------------- Pretty Printer ------------------------------

instance PPrint Spreadsheet where
  pprint sheet = unlines $ ["Formulas", pprint (formulas sheet), "Values", pprint (values sheet)]


-----------------------------------------------------------------
------------------- Lenses on cells -----------------------------

put3 :: Op -> Value -> Value -> Value
-- put3 o v1 v2 = v3 means v1 = (op o) v2 v3
put3 Plus  v1 v2 = (op Minus) v1 v2
put3 Minus v1 v2 = (op Minus) v2 v1
put3 Times v1 v2 = (op Div) v1 v2
put3 Div   v1 v2 = (op Div) v2 v1

get_scale Plus  v_old v_new = v_new / v_old
get_scale Minus v_old v_new = v_new / v_old
get_scale Times v_old v_new = sqrt (abs (v_new / v_old))
get_scale Div   v_old v_new = sqrt (abs (v_new / v_old))

get_default1 Plus  v_new = v_new / 2
get_default1 Minus v_new = v_new
get_default1 Times v_new = sqrt (abs v_new)
get_default1 Div   v_new = v_new

cell_get :: Formula -> SpreadsheetValues -> Value
cell_get (Cell new_name) values = Maybe.fromMaybe 0 $ Map.lookup new_name values
cell_get (BinOp o f1 f2) values = (op o) (cell_get f1 values) (cell_get f2 values)

cell_put :: Formula -> Value -> SpreadsheetValues -> SpreadsheetValues
cell_put (Cell target)       val values = Map.insert target val values
cell_put (BinOp o f1 f2) val values =
  let v1_old = cell_get f1 values in
  let v2_old = cell_get f2 values in
  -- compute old value of the formula
  let v_old = (op o) v1_old v2_old in
  -- compute v1_new, determine v2_new from v1_new
  let v1_new = 
        if v_old == 0 then
          get_default1 o val
        else
          let scale = get_scale o v_old val in
          scale * v1_old
  in
  let v2_new = put3 o val v1_new in
  cell_put f2 v2_new (cell_put f1 v1_new values)

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


--has_root :: DependencyGraph -> CellName -> Set CellName -> Maybe (Set CellName)
--has_root tree name seen =
  -- if name has been seen already then we followed a loop
--  if Set.member name seen then Nothing else
--    let seen_with_name = Set.insert name seen in
--    case Map.lookup name tree of
--      Just parent ->
--        case has_root tree parent seen_with_name of
--          Just seen' -> Just (Set.union seen_with_name seen')
--          Nothing    -> Nothing --has no root
--      Nothing     -> Just seen_with_name --has no parent, so name is a root

--check_forest :: DependencyGraph -> [CellName] -> Set CellName -> Bool
--check_forest tree [] _              = True
--check_forest tree (name:names) seen =
--  if Set.member name seen then
--    check_forest tree names seen
--  else
--    case has_root tree name Set.empty of
--      Just seen' -> check_forest tree names (Set.union seen seen')
--      Nothing    -> False



--     finalize should include a check that the dependency graph is acyclic
finalize formulas = 
  Just $ Spreadsheet formulas Map.empty $ build_dependencies formulas (Set.fromList (Map.keys formulas))
--  case build_dependencies formulas (Map.keys formulas) of
--    Just tree ->
--      if check_forest tree (Map.keys tree) Set.empty then
--         Just $ Spreadsheet formulas Map.empty tree
--      else
--        Nothing
--    Nothing   -> Nothing

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
  let parents = get_parents t siblings in
  -- call get on all the parents
  let v' = Set.fold g v parents where
        g parent v' = Map.insert parent value v' where
          value = Maybe.maybe 0 (\ formula -> cell_get formula v') (Map.lookup parent f) in
  if Set.null parents then v' else
    sweep_get f v' t parents
  
--Update the values of your children
sweep_put :: SpreadsheetFormulas -> SpreadsheetValues -> Set CellName -> SpreadsheetValues 
sweep_put f v siblings = Set.fold push v siblings where
  push name v = case (Map.lookup name v, Map.lookup name f) of
    (Just value, Just formula) ->
      let v' = cell_put formula value v in
      let children = get_children formula in
      sweep_put f v' children
    (_, _) -> v
  
