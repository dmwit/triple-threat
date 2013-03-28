{-# LANGUAGE FlexibleInstances #-}
module SpreadsheetPoly where

import Data.Packed
import Numeric.Container ((<>))
import Numeric.LinearAlgebra.Algorithms
import Numeric.LinearAlgebra.Util
import SpreadsheetCommon
import System.Environment
import System.IO

import qualified Data.Map as Map
import qualified Data.Set as Set

-- polynomials {{{1

type Weight = Double
data Polynomial = Polynomial
  { constant  :: Weight
  , monomials :: Map CellName Weight
  } deriving (Eq, Ord, Show, Read)

instance Num Polynomial where
  fromInteger n = Polynomial (fromInteger n) Map.empty
  Polynomial c ms + Polynomial c' ms' = Polynomial (c+c') (Map.unionWith (+) ms ms')
  negate (Polynomial c ms) = Polynomial (negate c) (negate <$> ms)

  (*)    = error "multiplication is not well-defined for this simple class of polynomials"
  abs    = error "absolute value is not well-defined for polynomials"
  signum = error "signum is not well-defined for polynomials"

instance PPrint Polynomial where
  pprint (Polynomial c ws) = intercalate "+" $
    [ show c | c /= 0 ] ++
    [ show w ++ "*<" ++ n ++ ">"
    | (n, w) <- Map.assocs ws
    , w /= 0
    ]

-- TODO: handle this more gracefully
smallOp (Infix Plus)  = (+)
smallOp (Infix Minus) = (-)

monomial c w = Polynomial 0 (Map.singleton c w)
fromDouble c = Polynomial c Map.empty
fromFormula (Cell n) = Polynomial 0 (Map.singleton n 1)
fromFormula (BinOp o f1 f2) = smallOp o (fromFormula f1) (fromFormula f2)

-- scalar multiplication and addition
v .* Polynomial c ms = Polynomial (v * c) ((v*) <$> ms)
v .+ p = fromDouble v + p

-- variable substitution for polynomials
subst :: Map CellName Polynomial -> Polynomial -> Polynomial
subst ps (Polynomial c ms) = c .+ sum
  [ maybe (monomial cell weight) (weight .*) (Map.lookup cell ps)
  | (cell, weight) <- Map.assocs ms
  ]

-- static check {{{1
type ExecutionPlan = ([CellName], Matrix Value)

-- Given a bunch of polynomials, check whether forall v, we can find values of
-- the input cells such that the polynomials output v. If so, return the
-- linear transformation that produces a minimal input delta given a minimal
-- output delta (and an appropriate order for the input names); if not,
-- Nothing.
--
-- This works by producing the linear transformation M corresponding to
-- applying all the polynomials at once, finding a basis N for the null space
-- of M, and outputting [M; N]^-1 * [I; 0] where I has as many rows as M does.
--
-- TODO: return an Either, with a counterexample showing one of the polynomials
-- being a linear combination of the others
planMultiUpdate_ :: [Polynomial] -> Maybe ExecutionPlan
planMultiUpdate_ ps = guard (length n == c-r) >> Just (roots, inv mn <> i0) where
	roots  = Set.toList . Set.unions . map (Map.keysSet . monomials) $ ps
	(r, c) = (length ps, length roots)
	m  = buildMatrix r c (\(p, r) -> Map.findWithDefault 0 (roots !! r) (monomials (ps !! p)))
	n  = nullspacePrec 1 m
	mn = fromBlocks [[m], [fromRows n]]
	i0 = fromBlocks [[diagl (replicate r 1)], [zeros (c-r) r]]

planMultiUpdate :: Spreadsheet -> [CellName] -> Maybe ExecutionPlan
planMultiUpdate s cs = planMultiUpdate_ (map (\n -> Map.findWithDefault (monomial n 1) n (summary s)) cs)
-- spreadsheets {{{1
type Summary = Map CellName Polynomial
data Spreadsheet = Spreadsheet
  { values       :: SpreadsheetValues
  , formulas     :: SpreadsheetFormulas
  , summary      :: Summary
  , dependencies :: Map CellName (Set CellName) -- if (k, vs) is in this map, when cell k changes, all the cells in vs should change by running the appropriate "get" function
  } deriving (Eq, Ord, Show, Read)

instance PPrint (Map CellName (Formula, Polynomial)) where
  pprint m = unlines
    [ cellName ++ " = " ++ pprint formula ++ " (= " ++ pprint polynomial ++ ")"
    | (cellName, (formula, polynomial)) <- Map.assocs m
    ]

instance PPrint Spreadsheet where
  pprint sheet
    = unlines ["Formulas", pprint (Map.intersectionWith (,) (formulas sheet) (summary sheet)),
               "Values", pprint (values sheet)
              ]

instance Parseable Spreadsheet where parser = finalize <$> parser

-- flatten formulas that reference cells with formulas into them into flat
-- polynomials that refer directly to base cells
finalize :: SpreadsheetFormulas -> Spreadsheet
finalize formulas = Spreadsheet values_ formulas_ summary_ dependencies_ where
  values_       = Map.empty
  formulas_     = formulas
  summary_      = eqFix (\m -> subst m <$> m) polynomials
  dependencies_ = Map.fromListWith Set.union $ do
    (target, Polynomial _ ms) <- Map.assocs summary_
    (source, _) <- Map.assocs ms
    return (source, Set.singleton target)

  polynomials = fromFormula <$> formulas

-- find the fixpoint of a function by iterating it and crossing our fingers
eqFix f x = let x' = f x in if x == x' then x else eqFix f x'

-- updates {{{1

-- not using SpreadsheetValues here when we expect to return a
-- differently-shaped map (i.e. one defined on a different domain of CellNames)
--
-- for now, put has one available spreading strategy: split the delta up
-- according to the magnitude of the weights on the monomials
get :: Polynomial -> SpreadsheetValues -> Value
put :: Polynomial -> Value -> SpreadsheetValues -> Map CellName Value
get p v = constant (subst (fromDouble <$> v) p)
put p@(Polynomial c ms) new v
  | factor == 0 = error "can't update the value of this cell"
  | otherwise   = Map.mapWithKey (\n w -> cell n + delta * abs w / factor) ms
  where
  old    = get p v
  delta  = new - old
  factor = sum [w * abs w | w <- Map.elems ms]
  cell n = Map.findWithDefault 0 n v

-- given some new values for the roots (that is, cells with no associated
-- formula), update the spreadsheet in the forward direction by running "get"
-- for all the cells that reference those roots; this is unsafe because we
-- don't check that the only values being set this way are actually roots
unsafeSetRoots :: Map CellName Value -> Spreadsheet -> Spreadsheet
unsafeSetRoots newRoots sheet = sheet { values = newValues } where
  allRoots  = newRoots `Map.union` values sheet
  newValues = gets `Map.union` allRoots
  nonRoots  = Set.unions
    [ Map.findWithDefault Set.empty n (dependencies sheet)
    | n <- Map.keys newRoots
    ]
  gets = Map.fromList
    [ (n, get (summary sheet Map.! n) allRoots)
    | n <- Set.toList nonRoots
    ]

-- do a full update: given a new value for a cell, change that cell, run put to
-- update all the roots that cell depends on, and then run all the gets
-- necessary to make the whole spreadsheet consistent
setValue :: CellName -> Value -> Spreadsheet -> Spreadsheet
setValue name value sheet = unsafeSetRoots newRoots sheet where
  polynomial = Map.findWithDefault (monomial name 1) name (summary sheet)
  newRoots   = put polynomial value (values sheet)

-- do a multi-update: given a pre-computed execution plan and a corresponding
-- mapping of output cells to updated values, run the execution plan to find
-- the new values of the root nodes and run all the gets necessary to make the
-- spreadsheet consistent
--
-- this is unsafe because it assumes that the execution plan's roots are
-- available in the spreadsheet, that is, that you've called plantRoots already
unsafeSetValues :: ExecutionPlan -> [(CellName, Value)] -> Spreadsheet -> Spreadsheet
unsafeSetValues (roots, transform) outputs sheet = unsafeSetRoots newRootMapping sheet where
	deltaOutputs   = [v - get (summary sheet Map.! n) (values sheet) | (n, v) <- outputs]
	deltaRoots     = toList . (transform <>) . fromList $ deltaOutputs
	oldRoots       = [get (summary sheet Map.! n) (values sheet) | n <- roots]
	newRoots       = zipWith (+) oldRoots deltaRoots
	newRootMapping = Map.fromList (zip roots newRoots)

-- ensure that an execution plan's roots all appear in the spreadsheet
plantRoots :: ExecutionPlan -> Spreadsheet -> Spreadsheet
plantRoots (roots, _) sheet = sheet { summary = summary sheet `Map.union` Map.fromList [(root, monomial root 1) | root <- roots] }

-- safe but possibly inefficient version of unsafeSetValues
setValues :: ExecutionPlan -> [CellName] -> [Value] -> Spreadsheet -> Spreadsheet
setValues plan names values = unsafeSetValues plan (zip names values) . plantRoots plan
-- IO {{{1

prompt s = do
  putStr s
  hFlush stdout
  getLine

readsWords = mapM noJunk . words where
  noJunk s = [v | (v, "") <- reads s]

setValueLoop s = do
  putStr (pprint s)
  names <- words <$> prompt "Change cells (space-separated): "
  case planMultiUpdate s names of
    Nothing   -> putStrLn "Those cells are too closely related." >> setValueLoop s
    Just plan -> do
      putStrLn $ "Okay, updating " ++ show names ++ " by modifying " ++ show (fst plan) ++ "."
      values_ <- prompt "Change the values to: "
      case readsWords values_ of
        [] -> putStrLn "That didn't look like numbers to me!" >> setValueLoop s
        values:_
          | length values /= length names -> putStrLn "Please give as many values as you gave names." >> setValueLoop s
          | otherwise -> setValueLoop (setValues plan names values s)

main = do
  a <- getArgs
  case a of
    [fileName] -> do
      s <- readFile fileName
      case parse parser fileName s of
        Left  err -> print err
        Right eqs -> setValueLoop eqs
    _ -> putStrLn "call with one argument naming a file with a bunch of equations, one on each line"
