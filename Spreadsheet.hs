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

data Spreadsheet = Spreadsheet
	{ formulas     :: SpreadsheetFormulas
	, values       :: SpreadsheetValues
	, dependencies :: DependencyGraph
	} deriving (Eq, Ord, Show, Read)

addFormula :: SpreadsheetFormulas -> CellName -> Formula -> SpreadsheetFormulas
finalize :: SpreadsheetFormulas -> Maybe Spreadsheet
step :: Spreadsheet -> CellName -> Value -> Spreadsheet

addFormula = undefined
finalize = undefined
step = undefined
