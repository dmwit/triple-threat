{-# LANGUAGE FlexibleInstances, FlexibleContexts, NoMonomorphismRestriction #-}
module Spreadsheet where

import Control.Applicative
import Data.Map
import qualified Data.Map as Map
import Data.Tree
import System.Environment
import Text.Parsec hiding (many, optional, (<|>))
import Data.Set
import qualified Data.Set as Set


type CellName = String
type Value    = Double
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
updateValue name val sheet = updateValues (Map.insert name val (values sheet)) sheet


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
    munge xs = Map.fromList [(n,f) | Just (Equation n f) <- xs]



-----------------------------------------------------------------
------------------- Lenses on cells -----------------------------

op Plus  = (+)
op Minus = (-)

cell_get :: Formula -> SpreadsheetValues -> Maybe Value
cell_get (Cell new_name) values = Map.lookup new_name values
cell_get (BinOp o f1 f2) values = liftA2 (op o) (cell_get f1 values) (cell_get f2 values)

cell_put :: Formula -> Value -> SpreadsheetValues -> SpreadsheetValues
cell_put (Cell target)       val values = Map.insert target val values
cell_put (BinOp Plus f1 f2)  val values =
  let v1_old = cell_get f1 values in
  let v2_old = cell_get f2 values in
  case (v1_old,v2_old) of
    (Just v1_old,Just v2_old) ->
      let v1_new = (val * v1_old) / (v1_old + v2_old) in
      let v2_new = val - v1_new in
      let values' = cell_put f1 v1_new values in
      cell_put f2 v2_new values'
    (_,_) ->
      let v1_new = val / 2 in
      let v2_new = val - v1_new in
      let values' = cell_put f1 v1_new values in
      cell_put f2 v2_new values'
cell_put (BinOp Minus f1 f2) val values =
  let v1_old = cell_get f1 values in
  let v2_old = cell_get f2 values in
  case (v1_old,v2_old) of
    (Just v1_old,Just v2_old) ->
      let v1_new = (val * v1_old) / (v1_old - v2_old) in
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
build_dependencies formulas []        = Just Map.empty
build_dependencies formulas (name:ls) =
  case build_dependencies formulas ls of
    Just tr -> 
      let children = case Map.lookup name formulas of
            Just formula -> get_children formula
            Nothing      -> []
      in
       add_parent tr children name
    Nothing -> Nothing

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

add_parent tr []         _       = Just tr
add_parent tr (child:ls) parent  =
  case add_parent tr ls parent of
    Just tr' ->
      if Map.member child tr' then
         Nothing --not a tree
      else
         Just $ Map.insert child parent tr'
    Nothing  -> Nothing

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
  case build_dependencies formulas (keys formulas) of
    Just tree ->
      if check_forest tree (keys tree) Set.empty then
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



main = do
  a <- getArgs
  case a of
    [fileName] -> do
      s <- readFile fileName
      case parse parser fileName s of
        Left  err -> print err
        Right eqs -> putStr . pprint $ (eqs :: SpreadsheetFormulas)
    _ -> putStrLn "call with one argument naming a file with a bunch of equations, one on each line"
