{-# LANGUAGE FlexibleContexts, FlexibleInstances, NoMonomorphismRestriction #-}
module SpreadsheetCommon
  ( module SpreadsheetCommon
  , module Control.Applicative
  , module Control.Monad
  , module Data.List
  , module Text.Parsec
  , Map
  , Set.Set
  ) where

import Control.Applicative
import Control.Monad
import Data.List
import Data.Map
import qualified Data.Set as Set
import Text.Parsec hiding (many, optional, (<|>))

type CellName = String
type Value    = Double
type SpreadsheetFormulas = Map CellName Formula
type SpreadsheetValues   = Map CellName Value
--DependencyGraph: Each cell gets mapped to the set of its parents
type DependencyGraph     = Map CellName (Set.Set CellName)

data Formula
  = Cell CellName
  | BinOp Op Formula Formula
  deriving (Eq, Ord, Show, Read)

data Equation = Equation CellName Formula deriving (Eq, Ord, Show, Read)

data Op = Plus | Minus | Times | Div deriving (Eq, Ord, Show, Read)

op Plus  = (+)
op Minus = (-)
op Times = (*)
op Div   = (/)

-----------------------------------------------------------------
------------------- Pretty Printer ------------------------------

class PPrint a where pprint :: a -> String

instance PPrint SpreadsheetFormulas where
  pprint m = unlines [cellName ++ " = " ++ pprint formula | (cellName, formula) <- assocs m]

instance PPrint SpreadsheetValues where
  pprint m = unlines [cellName ++ " = " ++ pprint value   | (cellName, value  ) <- assocs m]

instance PPrint Value where pprint = show
instance PPrint [CellName] where pprint cells = intercalate ", " cells

instance PPrint Formula where
  pprint (Cell s) = "<" ++ s ++ ">"
  pprint (BinOp op f1 f2) = "(" ++ pprint f1 ++ pprint op ++ pprint f2 ++ ")"

instance PPrint Op where
  pprint Plus  = "+"
  pprint Minus = "-"
  pprint Times = "*"
  pprint Div   = "/"

instance PPrint DependencyGraph where
  pprint m = unlines [cellName ++ " = " ++ pprint namesList | (cellName, namesList) <- assocs m]

instance PPrint (Set.Set CellName) where
  pprint s = pprint (Set.elems s)

-----------------------------------------------------------------
---------------------------- Parser -----------------------------

parseCellName = many1 (noneOf " >")
parseConst    = many1 (noneOf "#")

class Parseable a where
  parser :: Stream s m Char => ParsecT s u m a

instance Parseable Op where
  parser = choice
    [ "+" --> Plus
    , "-" --> Minus
    , "*" --> Times
    , "/" --> Div
    ] where s --> o = string s >> return o

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
