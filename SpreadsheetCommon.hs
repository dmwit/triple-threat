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
import Data.List hiding (lookup)
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

data InOp = Plus | Minus | Times | Div | Pow deriving (Eq, Ord, Show, Read)
data PreOp = Min | Max deriving (Eq, Ord, Show, Read)
data Op = Infix InOp | Prefix PreOp  deriving (Eq, Ord, Show, Read)

op (Infix Plus)  = (+)
op (Infix Minus) = (-)
op (Infix Times) = (*)
op (Infix Div)   = (/)
op (Infix Pow)   = (**)
op (Prefix Min)  = min
op (Prefix Max)  = max

-----------------------------------------------------------------
------------------- Pretty Printer ------------------------------

class PPrint a where pprint :: a -> String

instance PPrint SpreadsheetFormulas where
  pprint m = unlines [cellName ++ " = " ++ pprint formula | (cellName, formula) <- assocs m]

instance PPrint SpreadsheetValues where
  pprint m = unlines [cellName ++ " = " ++ pprint value   | (cellName, value  ) <- assocs m]

instance PPrint Value where pprint = show
instance PPrint [CellName] where pprint cells = intercalate " " cells

instance PPrint Formula where
  pprint (Cell s) = "<" ++ s ++ ">"
  pprint (BinOp (Infix  o) f1 f2) = "(" ++ pprint f1 ++ pprint o ++ pprint f2 ++ ")"
  pprint (BinOp (Prefix o) f1 f2) = pprint o ++ "(" ++ pprint f1 ++ "," ++ pprint f2 ++ ")"

instance PPrint InOp where
  pprint Plus  = "+"
  pprint Minus = "-"
  pprint Times = "*"
  pprint Div   = "/"
  pprint Pow   = "^"

instance PPrint PreOp where
  pprint Min   = "min"
  pprint Max   = "max"

instance PPrint DependencyGraph where
  pprint m = unlines [cellName ++ " = " ++ pprint namesList | (cellName, namesList) <- assocs m]

instance PPrint (Set.Set CellName) where
  pprint s = pprint (Set.elems s)
  

instance PPrint (Set.Set (Set.Set CellName)) where
  pprint s = intercalate ", " $ Data.List.map pprint (Set.elems s)
  
  
instance PPrint Equation where
  pprint (Equation name f) = name ++ " = " ++ pprint f
  
instance PPrint (Set.Set Equation) where
  pprint s = intercalate "; " $ Data.List.map pprint (Set.elems s)


 -----------------------------------------------------------------
---------------------------- Parser -----------------------------

parseCellName = many1 (noneOf " >")
parseConst    = many1 (noneOf "#")

class Parseable a where
  parser :: Stream s m Char => ParsecT s u m a

instance Parseable InOp where
  parser = choice
    [ "+" --> Plus
    , "-" --> Minus
    , "*" --> Times
    , "/" --> Div
    , "^" --> Pow
    ] where s --> o = string s >> return o

instance Parseable PreOp where
  parser = choice
    [ "min" --> Min
    , "max" --> Max
    ] where s --> o = try (string s) >> return o


instance Parseable Formula where
  parser = chainl1 (parens <|> cell) (BinOp . Infix <$> parser) <|> prefix_exp where
    prefix_exp = BinOp . Prefix <$> parser <* string "(" <*> parser <* string "," <*> parser <* string ")"
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
