{-# LANGUAGE FlexibleInstances, FlexibleContexts, NoMonomorphismRestriction #-}
module Spreadsheet where

import Control.Applicative
import Data.Map
import Data.Tree
import System.Environment
import Text.Parsec hiding (many, optional, (<|>))

type CellName = String
type Value    = Integer
type SpreadsheetFormulas = Map CellName Formula
type SpreadsheetValues   = Map CellName Value
type DependencyGraph     = Map CellName [CellName]

data Formula
	= Cell CellName
	| BinOp Op Formula Formula
	deriving (Eq, Ord, Show, Read)

data Equation = Equation CellName Formula deriving (Eq, Ord, Show, Read)

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

main = do
	a <- getArgs
	case a of
		[fileName] -> do
			s <- readFile fileName
			case parse parser fileName s of
				Left  err -> print err
				Right eqs -> putStr . pprint $ (eqs :: SpreadsheetFormulas)
		_ -> putStrLn "call with one argument naming a file with a bunch of equations, one on each line"
