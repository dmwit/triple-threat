{-# LANGUAGE FlexibleInstances, FlexibleContexts, NoMonomorphismRestriction #-}
module Spreadsheet where

import Control.Applicative
import Data.Map
import qualified Data.Map as Map
import Data.Tree
import System.Environment
import Text.Parsec hiding (many, optional, (<|>))


type CellName = String
type Value    = Integer
type SpreadsheetFormulas = Map CellName Formula
type SpreadsheetValues   = Map CellName Value
type DependencyGraph     = Map CellName [CellName]
--DependencyTree: Each cell gets mapped to its parent, if it has one
type DependencyTree      = Map CellName CellName

data Formula
	= Cell CellName
	| BinOp Op Formula Formula
	deriving (Eq, Ord, Show, Read)

data Equation = Equation CellName Formula deriving (Eq, Ord, Show, Read)

data Op = Plus | Minus deriving (Eq, Ord, Show, Read)


data Spreadsheet = Spreadsheet
	{ formulas     :: SpreadsheetFormulas
	, values       :: SpreadsheetValues
	, dependencies :: DependencyTree
	} deriving (Eq, Ord, Show, Read)
                   
updateValues :: SpreadsheetValues -> Spreadsheet -> Spreadsheet
updateValues new_values sheet = Spreadsheet (formulas sheet) new_values (dependencies sheet)
updateValue :: CellName -> Value -> Spreadsheet -> Spreadsheet
updateValue name val sheet = updateValues (insert name val (values sheet)) sheet


----------------------------------------------------------------- 
------------------- Pretty Printer ------------------------------

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
        
instance PPrint DependencyTree where
        pprint m = unlines [cellName ++ " = " ++ name | (cellName, name) <- assocs m]
        
instance PPrint [CellName] where
        pprint []            = ""
        pprint (cellName:ls) = cellName ++ ", " ++ pprint ls
        
instance PPrint Spreadsheet where
        pprint sheet = unlines $ ["Formulas", pprint (formulas sheet), "Values", pprint (values sheet)]
        
        
----------------------------------------------------------------- 
---------------------------- Parser -----------------------------

parseCellName = many1 (noneOf " >")

class    Parseable a       where parser :: Stream s m Char => ParsecT s u m a
instance Parseable Op      where parser = (string "+" >> return Plus) <|> (string "-" >> return Minus)
instance Parseable Formula where
	parser = chainl1 (parens <|> cell) (BinOp <$> parser) where
		cell   = string "<" *> (Cell <$> parseCellName) <* string ">"
		parens = string "(" *> parser <* string ")"

instance Parseable Equation where
	parser = do
		n <- parseCellName
		string " = "
		f <- parser
		return (Equation n f)

instance Parseable SpreadsheetFormulas where
	parser = munge <$> many (optional parser <* string "\n") where
		munge xs = fromList [(n,f) | Just (Equation n f) <- xs]

                   

----------------------------------------------------------------- 
------------------- Lenses on cells -----------------------------

cell_get :: Formula -> SpreadsheetValues -> Maybe Value
cell_get (Cell new_name)     values = Map.lookup new_name values
cell_get (BinOp Plus f1 f2)  values = do
  v1 <- cell_get f1 values
  v2 <- cell_get f2 values
  return (v1 + v2)
cell_get (BinOp Minus f1 f2) values = do
  v1 <- cell_get f1 values                                    
  v2 <- cell_get f2 values
  return (v1 - v2)

cell_put :: Formula -> Value -> SpreadsheetValues -> SpreadsheetValues
cell_put (Cell target)       val values = insert target val values
cell_put (BinOp Plus f1 f2)  val values = 
  let v1_old = cell_get f1 values in
  let v2_old = cell_get f2 values in
  case (v1_old,v2_old) of
    (Just v1_old,Just v2_old) -> 
      let v1_new = quot (val * v1_old) (v1_old + v2_old) in
      let v2_new = val - v1_new in
      let values' = cell_put f1 v1_new values in
      cell_put f2 v2_new values'
    (_,_) -> 
      let v1_new = quot val 2 in
      let v2_new = val - v1_new in
      let values' = cell_put f1 v1_new values in
      cell_put f2 v2_new values'
cell_put (BinOp Minus f1 f2) val values =
  let v1_old = cell_get f1 values in
  let v2_old = cell_get f2 values in
  case (v1_old,v2_old) of
    (Just v1_old,Just v2_old) -> 
      let v1_new = quot (val * v1_old) (v1_old - v2_old) in
      let v2_new = val - v1_new in
      let values' = cell_put f1 v1_new values in
      cell_put f2 v2_new values'
    (_,_) -> 
      let v1_new = val in
      let v2_new = 0 in
      let values' = cell_put f1 v1_new values in
      cell_put f2 v2_new values'
                                   
                             
--------------------------------------------------------------------
--------------------- Evaluation -----------------------------------
                   
-- 1 -- add all formulas
addFormula :: SpreadsheetFormulas -> CellName -> Formula -> SpreadsheetFormulas
addFormula formulas name formula = insert name formula formulas

-- 2 -- build a dependency graph from the formulas given, and check
--      that the graph forms a tree
finalize :: SpreadsheetFormulas -> Maybe Spreadsheet
build_dependencies :: SpreadsheetFormulas -> [CellName] -> DependencyTree
build_backwards_dependencies :: SpreadsheetFormulas -> [CellName] -> DependencyGraph
get_children :: Formula -> [CellName]
add_parent :: DependencyTree -> [CellName] -> CellName -> DependencyTree
add_children :: DependencyGraph -> [CellName] -> CellName -> DependencyGraph

-- Builds a DependencyTree where every cell points to its parent,
-- a node whose formula points to it.
build_dependencies formulas []        = Map.empty
build_dependencies formulas (name:ls) = 
  let tr       = build_dependencies formulas ls in
  let children = case Map.lookup name formulas of
                      Just formula -> get_children formula
                      Nothing      -> []
  in
  add_parent tr children name
  
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

add_parent tr []         _       = tr
add_parent tr (child:ls) parent  = 
  let tr'    = add_parent tr ls parent in
  insert child parent tr'
  
add_children gr [] _ = gr
add_children gr (child:ls) parent =
  let gr' = add_children gr ls parent in
  case Map.lookup parent gr' of
       Just siblings -> insert parent (child:siblings) gr'
       Nothing       -> insert parent [child]          gr'
       
  
  
--     finalize should include a check that the dependency graph is acyclic
finalize formulas = 
  let tree = build_dependencies formulas (keys formulas) in
  Just $ Spreadsheet formulas Map.empty tree

-- 3 -- edit values in the spreadsheet to populate the spreadsheet
step :: Spreadsheet -> CellName -> Value -> Spreadsheet
step sheet name val = 
  let f = formulas sheet in
  let v = values sheet in
  let t = dependencies sheet in
  -- add value to the spreadsheet's values
  let v' = insert name val v in
  -- push down then push up
  updateValues (push_up f (push_down f v' [name]) t name) sheet
  
-- should add a check: if the operation doesn't do anything, do not recurse up (or down)
push_up :: SpreadsheetFormulas -> SpreadsheetValues -> DependencyTree -> CellName -> SpreadsheetValues
push_up f v t name =
  case Map.lookup name t of
    Just parent -> case Map.lookup parent f of
        Just formula -> case cell_get formula v of
            Just value -> 
              let v' = insert parent value v in
              push_up f v' t parent
            Nothing    -> --If we can't calculate the value of a cell, clear it and push that upwards
              let v' = delete parent v in
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
   


main = do
	a <- getArgs
	case a of
		[fileName] -> do
			s <- readFile fileName
			case parse parser fileName s of
				Left  err -> print err
				Right eqs -> putStr . pprint $ (eqs :: SpreadsheetFormulas)
		_ -> putStrLn "call with one argument naming a file with a bunch of equations, one on each line"
