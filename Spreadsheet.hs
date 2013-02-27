{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module Spreadsheet where

import Data.Map
import Data.Tree


type CellName = String
type Value    = Integer
type SpreadsheetFormulas = Map CellName Formula
type SpreadsheetValues   = Map CellName Value
type DependencyGraph     = Map CellName [CellName]

data Formula
	= Cell CellName
	| BinOp Op Formula Formula
	deriving (Eq, Ord, Show, Read)

data Op = Plus | Minus deriving (Eq, Ord, Show, Read)


-- pretty printer 
class PPrint a where pprint :: a -> String

instance PPrint SpreadsheetFormulas where
	pprint m = unlines [cellName ++ " = " ++ pprint formula | (cellName, formula) <- assocs m]

instance PPrint SpreadsheetValues where
	pprint m = unlines [cellName ++ " = " ++ pprint value   | (cellName, value  ) <- assocs m]
        
instance PPrint Value where pprint = show

instance PPrint Formula where
	pprint (Cell s) = "<" ++ s ++ ">"
	pprint (BinOp op f1 f2) = "(" ++ pprint f1 ++ pprint op ++ pprint f2 ++ ")"

instance PPrint Op where
	pprint Plus  = "+"
	pprint Minus = "-"
        
instance PPrint DependencyGraph where
        pprint m = unlines [cellName ++ " = " ++ pprint namesList | (cellName, namesList) <- assocs m]
        
instance PPrint [CellName] where
        pprint []            = ""
        pprint (cellName:ls) = cellName ++ pprint ls

data Spreadsheet = Spreadsheet
	{ formulas     :: SpreadsheetFormulas
	, values       :: SpreadsheetValues
	, dependencies :: DependencyGraph
	} deriving (Eq, Ord, Show, Read)
                   
updateValues :: SpreadsheetValues -> Spreadsheet -> Spreadsheet
updateValues new_values sheet = Spreadsheet (formulas sheet) new_values (dependencies sheet)
updateValue :: CellName -> Value -> Spreadsheet -> Spreadsheet
updateValue name val sheet = updateValues (insert name val (values sheet)) sheet
                   

----------------------------------------------------------------- 
------------------- Lenses on cells -----------------------------

cell_get :: Formula -> SpreadsheetValues -> Value
cell_get (Cell new_name)     values = values ! new_name
cell_get (BinOp Plus f1 f2)  values = cell_get f1 values + cell_get f2 values
cell_get (BinOp Minus f1 f2) values = cell_get f1 values - cell_get f2 values

cell_put :: Formula -> Value -> SpreadsheetValues -> SpreadsheetValues
cell_put (Cell target)      val values = insert target val values
cell_put (BinOp Plus f1 f2) val values = 
  let v1_old = cell_get f1 values in
  let v2_old = cell_get f2 values in
  let v1_new = quot (val * v1_old) (v1_old + v2_old) in
  let v2_new = val - v1_new in
  let values' = cell_put f1 v1_new values in
  cell_put f2 v2_new values'
  -- add MINUS
                                   
                             
--------------------------------------------------------------------
--------------------- Evaluation -----------------------------------
                   
-- 1 -- add all formulas
addFormula :: SpreadsheetFormulas -> CellName -> Formula -> SpreadsheetFormulas
addFormula formulas name formula = insert name formula formulas

-- 2 -- build a dependency graph from the formulas given, and check
--      that the graph forms a tree
finalize :: SpreadsheetFormulas -> Maybe Spreadsheet
build_dependencies :: SpreadsheetFormulas -> [CellName] -> DependencyGraph
get_children :: Formula -> [CellName]
add_parent :: DependencyGraph -> [CellName] -> CellName -> DependencyGraph

build_dependencies formulas []         = fromList []
build_dependencies formulas (name:ls) = 
  let gr       = build_dependencies formulas ls in
  let children = get_children (formulas ! name) in
  add_parent gr children name
  
get_children (Cell name)     = [name]
get_children (BinOp _ f1 f2) = get_children f1 ++ get_children f2

add_parent gr [] _ = gr
add_parent gr (child:ls) parent  = 
  let gr'    = add_parent gr ls parent in
  let newval = if member child gr' then parent:(gr' ! child) else [parent] in
  insert child newval gr'
  
--     finalize should include a check that the dependency graph is
--     either acyclic or a tree
finalize formulas = Just $ Spreadsheet formulas empty (build_dependencies formulas (keys formulas))

-- 3 -- edit values in the spreadsheet to populate the spreadsheet
step :: Spreadsheet -> CellName -> Value -> Spreadsheet
step sheet name val = 
  -- add value to the spreadsheet's values
  let new_sheet = updateValue name val sheet in
  -- step down then step up
  step_up (step_down new_sheet name) name

--- step_up: look at the dependencies of the node's parents
step_up :: Spreadsheet -> CellName -> Spreadsheet
step_up sheet name = 
  let parents = if member name (dependencies sheet) then (dependencies sheet) ! name else [] in
  -- for parent in parents, calculate the cell parent whose formula is given by (formulas ! parent)
  let new_values = calculate_cells (formulas sheet) (values sheet) parents in
  updateValues new_values sheet
  
--- calculate_cells: recalculate the cells given by the list
calculate_cells :: SpreadsheetFormulas -> SpreadsheetValues -> [CellName] -> SpreadsheetValues
calculate_cells _ values [] = values
calculate_cells formulas values (name:names) = 
  let values' = calculate_cells formulas values names in
  let formula = formulas ! name in
  let value = cell_get formula values' in
  insert name value values'
   
--- step_down : look at the children of a node
step_down :: Spreadsheet -> CellName -> Spreadsheet
step_down sheet name = 
  let val = (values sheet) ! name in
  let formula = (formulas sheet) ! name in
  let new_values = cell_put formula val (values sheet) in
  updateValues new_values sheet



  
  
