module Spreadsheet where

import SpreadsheetCommon
import qualified Data.Map as Map
import qualified Data.Set as Set


data Spreadsheet = Spreadsheet
  { formulas     :: SpreadsheetFormulas
  , values       :: SpreadsheetValues
  , dependencies :: DependencyTree
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

get_scale Plus v_old v_new = 
  if v_old == 0 then v_new / 2 else v_new / v_old
get_scale Minus v_old v_new =
  if v_old == 0 then v_new else v_new / v_old
get_scale Times v_old v_new = 
  if v_old == 0 then sqrt (abs (v_new)) else sqrt (abs (v_new / v_old))
get_scale Div v_old v_new =
  if v_old == 0 then v_new else sqrt (abs (v_new / v_old))

cell_get :: Formula -> SpreadsheetValues -> Maybe Value
cell_get (Cell new_name) values = Map.lookup new_name values
cell_get (BinOp o f1 f2) values = liftA2 (op o) (cell_get f1 values) (cell_get f2 values)

cell_put :: Formula -> Value -> SpreadsheetValues -> SpreadsheetValues
cell_put (Cell target)       val values = Map.insert target val values
cell_put (BinOp o f1 f2) val values =
  let v1_old = cell_get f1 values in
  let v2_old = cell_get f2 values in
  case (v1_old,v2_old) of
    (Just v1_old,Just v2_old) ->
      let v_old = (op o) v1_old v2_old in
      let scale = get_scale o v_old val in
      let v1_new = scale * v1_old in
      let v2_new = put3 o val v1_new in
      let values' = cell_put f1 v1_new values in
      cell_put f2 v2_new values'
    (_,_) ->
      let v1_new = get_scale o 0 val in
      let v2_new = put3 o val v1_new in
      let values' = cell_put f1 v1_new values in
      cell_put f2 v2_new values'


--------------------------------------------------------------------
--------------------- Evaluation -----------------------------------

-- 1 -- add all formulas
addFormula :: SpreadsheetFormulas -> CellName -> Formula -> SpreadsheetFormulas
addFormula formulas name formula = Map.insert name formula formulas

-- 2 -- build a dependency graph from the formulas given, and check
--      that the graph forms a tree
finalize :: SpreadsheetFormulas -> Maybe Spreadsheet
build_dependencies :: SpreadsheetFormulas -> [CellName] -> Maybe DependencyTree
build_backwards_dependencies :: SpreadsheetFormulas -> [CellName] -> DependencyGraph
get_children :: Formula -> [CellName]
add_parent :: DependencyTree -> [CellName] -> CellName -> Maybe DependencyTree
add_children :: DependencyGraph -> [CellName] -> CellName -> DependencyGraph

-- Builds a DependencyTree where every cell points to its parent,
-- a node whose formula points to it.
build_dependencies formulas names = foldM f Map.empty names where
  children_of name = maybe [] get_children (Map.lookup name formulas)
  f tr name = add_parent tr (children_of name) name

-- Builds a DependencyGraph where every cell points to its children
build_backwards_dependencies formulas []        = Map.empty
build_backwards_dependencies formulas (name:ls) =
  let gr       = build_backwards_dependencies formulas ls in
  let children = case Map.lookup name formulas of
                      Just formula -> get_children formula
                      Nothing      -> []
  in
  add_children gr children name


get_children (Cell name)     = [name]
get_children (BinOp _ f1 f2) = get_children f1 ++ get_children f2

add_parent tr children parent = foldM f tr children where
  f tr child = do
    guard $ not (Map.member child tr)
    return (Map.insert child parent tr)

add_children gr ls parent = Map.insertWith (++) parent ls gr


has_root :: DependencyTree -> CellName -> Set CellName -> Maybe (Set CellName)
has_root tree name seen =
  -- if name has been seen already then we followed a loop
  if Set.member name seen then Nothing else
    let seen_with_name = Set.insert name seen in
    case Map.lookup name tree of
      Just parent ->
        case has_root tree parent seen_with_name of
          Just seen' -> Just (Set.union seen_with_name seen')
          Nothing    -> Nothing --has no root
      Nothing     -> Just seen_with_name --has no parent, so name is a root

check_forest :: DependencyTree -> [CellName] -> Set CellName -> Bool
check_forest tree [] _              = True
check_forest tree (name:names) seen =
  if Set.member name seen then
    check_forest tree names seen
  else
    case has_root tree name Set.empty of
      Just seen' -> check_forest tree names (Set.union seen seen')
      Nothing    -> False



--     finalize should include a check that the dependency graph is acyclic
finalize formulas =
  case build_dependencies formulas (Map.keys formulas) of
    Just tree ->
      if check_forest tree (Map.keys tree) Set.empty then
         Just $ Spreadsheet formulas Map.empty tree
      else
        Nothing
    Nothing   -> Nothing

-- 3 -- edit values in the spreadsheet to populate the spreadsheet
step :: Spreadsheet -> CellName -> Value -> Spreadsheet
step sheet name val =
  let f = formulas sheet in
  let v = values sheet in
  let t = dependencies sheet in
  -- add value to the spreadsheet's values
  let v' = Map.insert name val v in
  -- push down then push up
  updateValues (push_up f (push_down f v' [name]) t name) sheet

-- should add a check: if the operation doesn't do anything, do not recurse up (or down)
push_up :: SpreadsheetFormulas -> SpreadsheetValues -> DependencyTree -> CellName -> SpreadsheetValues
push_up f v t name =
  case Map.lookup name t of
    Just parent -> case Map.lookup parent f of
        Just formula -> case cell_get formula v of
            Just value ->
              if Map.lookup parent v == Just value 
              then v 
              else 
                let v' = Map.insert parent value v in
                push_up f v' t parent
            Nothing    -> --If we can't calculate the value of a cell, clear it and push that upwards
              let v' = Map.delete parent v in
              push_up f v' t parent
        Nothing      -> error ("Parent must have a formula for it to have a child in the dependency tree")
    Nothing     -> v

--Update the values of your children
push_down :: SpreadsheetFormulas -> SpreadsheetValues -> [CellName] -> SpreadsheetValues
push_down f v [] = v
push_down f v (name:siblings) =
  let v' = case (Map.lookup name v, Map.lookup name f) of
        (Just value, Just formula) ->
          let v'' = cell_put formula value v in
          let children = get_children formula in
          push_down f v'' children
        (_         , _           ) -> v
      in
   push_down f v' siblings

