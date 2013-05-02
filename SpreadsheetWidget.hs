{-# LANGUAGE FlexibleInstances #-}
module SpreadsheetWidget where

import SpreadsheetCommon
import GetPut
import Data.Map (Map)
import Data.Set (Set)
import System.Environment
import System.IO

import Control.Applicative
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import Text.Parsec hiding (many, optional, (<|>))

type CellDomain = Set CellName
type DangerZone = Set CellDomain
type Method     = SpreadsheetValues -> SpreadsheetValues

data Widget = Widget
  { domain    :: CellDomain
  , invariant :: SpreadsheetValues -> Bool
  , danger    :: DangerZone
  , methods   :: CellDomain -> Method
  }
  

-- Can also add inequalities at a later date
data Relation = Eq Formula Formula | And Relation Relation
  deriving (Eq, Ord, Show, Read)
  
-- Define RelationWidgets in terms of equations and relations we can read,
-- then translate them into methods and invariants
data RelationWidget = RelationWidget
  { relation  :: Relation
  , equations :: Map CellDomain (Set Equation)
  , dangerZone:: DangerZone
  }
  

-----------------------------------------------------------------
------------------- Pretty Printer ------------------------------
  
instance PPrint Relation where
  pprint (Eq f1 f2) = pprint f1 ++ " = " ++ pprint f2
  pprint (And r1 r2) = pprint r1 ++ " /\\ " ++ pprint r2 
  

printEqns m = unlines [pprint dom ++ " : " ++ pprint eq_set | (dom, eq_set) <- Map.assocs m]

--instance PPrint (Map CellDomain (Set Equation)) where
--  pprint m = unlines [ pprint dom ++ " : " ++ pprint eq_set | (dom, eq_set) <- Map.assocs m]
  

-------------------------------------------------------------------------
----------------------------- Composition -------------------------------
  
restrictDom :: SpreadsheetValues -> CellDomain -> SpreadsheetValues
restrictDom vals dom = -- Map.intersection vals . Map.fromSet (const ())
  Map.intersection vals (Map.fromList $ List.map (\ x -> (x,1)) (Set.elems dom))

safe, dangerous :: DangerZone -> Set CellName -> Bool
safe = (not .) . dangerous
dangerous rep s = any (`Set.isSubsetOf` s) (Set.toList rep)

-- Minimize the representation of a danger zone.
-- Given a representation rep, this function attempts to produce a smaller
-- representation rep' such that dangerous rep = dangerous rep'. At the moment,
-- it does this only by iterating one rule: remove any sets of names which are
-- proper supersets of another set of names in the representation. To be
-- complete, we would probably also need a rule that said something like "If
-- all of the sets of names S U {x} for x in the domain of the lens are in the
-- representation, replace them with the single set S.". We don't try to do
-- that yet.
minimizeDangerZone x = Set.filter (\s -> and [not (Set.isProperSubsetOf s' s) | s' <- Set.toList x]) x

compose :: Widget -> Widget -> Widget
compose
  ( Widget { domain = domk, invariant = invk, danger = dzk, methods = fk })
  ( Widget { domain = doml, invariant = invl, danger = dzl, methods = fl })
  = Widget { domain = dom , invariant = inv , danger = dz , methods = f  } where
  dom      = Set.union domk doml
  inv vals = invk (restrictDom vals domk) && invl (restrictDom vals doml)
  shared   = domk `Set.intersection` doml
  dz       = minimizeDangerZone (dzk `Set.union` dzl `Set.union` Set.fromList
                                   [ (vk `Set.union` vl) `Set.difference` shared
                                   | vk <- Set.toList dzk
                                   , vl <- Set.toList dzl
                                   ])
  f inputs = if canRunKFirst then runKFirst else runLFirst where
    kIn = Set.intersection inputs domk
    lIn = Set.intersection inputs doml
    kInShared = Set.union kIn shared
    lInShared = Set.union lIn shared

    canRunKFirst   = safe dzk kIn && safe dzl lInShared
    runKFirst vals = let
      vals'  = fk kIn (restrictDom vals domk)
      vals'' = fl lInShared (restrictDom vals' doml `Map.union` restrictDom vals doml)
      in vals' `Map.union` vals''
    runLFirst vals = let
      vals'  = fl lIn (restrictDom vals doml)
      vals'' = fk kInShared (restrictDom vals' domk `Map.union` restrictDom vals domk)
      in vals' `Map.union` vals''


-------------------------------------------------------------------------
--------------------------------- Step ----------------------------------

step :: Widget -> SpreadsheetValues -> SpreadsheetValues -> SpreadsheetValues
step (Widget { domain = dom , invariant = inv , danger = dz , methods = f  }) 
     vals input =
  let i = Map.keysSet input in
  if dangerous dz i
  then vals 
  else 
    let vals' = Map.union input vals in
    f i vals'
    

-----------------------------------------------------------------
--------------------- RelationWidget to Widgets -----------------
  
getDomain :: RelationWidget -> CellDomain 
getDomain w = Set.unions $ Map.keys (equations w)

getInvariant :: Relation -> SpreadsheetValues -> Bool
getInvariant (Eq f1 f2) vals = 
  let mv1 = cell_get_maybe f1 vals in
  let mv2 = cell_get_maybe f2 vals in
  Maybe.fromMaybe False $ liftM2 (==) mv1 mv2
  
getEquationMethod :: Equation -> Method
getEquationMethod (Equation target (Cell name)) = -- let target = name
  \ vals -> Map.alter (\ _ -> Map.lookup name vals) target vals
getEquationMethod (Equation target f) = -- let target = f
  \ vals -> cell_put f (Map.lookup target vals) vals
            
getMethods :: Map CellDomain (Set Equation) -> CellDomain -> Method
getMethods eqns dom = 
  let dom_eqns = Maybe.fromMaybe Set.empty $ Map.lookup dom eqns in
  Set.fold f id dom_eqns where
    f eq m = \ vals -> getEquationMethod eq (m vals)


getWidget :: RelationWidget -> Widget
getWidget w = Widget { domain = dom , invariant = inv , danger = dz , methods = f  } where
  dom = getDomain w 
  inv = getInvariant $ relation w 
  dz  = dangerZone w
  f   = getMethods $ equations w



-------------------------------------------------------------------------
------------------------------ Parser -----------------------------------
    
instance Parseable RelationWidget where
  parser = do
    rel <- parser -- relation
    string "\n"
    dz <- parser -- danger zone
    string "\n"
    eqns <- parser -- equations
    return (RelationWidget { relation = rel, dangerZone = dz, equations = eqns })
    
instance Parseable Widget where
  parser = getWidget <$> parser -- RelationWidget
    
        
instance Parseable Relation where
  parser = do
    f1 <- parser -- formula
    string " = " 
    f2 <- parser -- formula
    return (Eq f1 f2)
    
    
instance Parseable DangerZone where
  parser = munge <$> many (optional parser <* ( string ", " <|> string "\n" )) where -- parser here : CellDomain
    munge xs = Set.fromList [ x | Just x <- xs ]
  
instance Parseable (Map CellDomain (Set Equation)) where
  parser = munge <$> many (optional parser <* string "\n") where -- parser here: (CellDomain, Set Equation)
    munge xs = Map.fromList [(i,e) | Just(i,e) <- xs]
    
instance Parseable (Set Equation) where
  parser = munge <$> many (optional parser <* (string ", " <|> string "\n")) where -- parser here : Equation
    munge xs = Set.fromList [ x | Just x <- xs ]
    
instance Parseable (CellDomain,Set Equation) where
  parser = do 
    i <- parser -- CellDomain 
    string " : "
    e <- parser -- Set Equation
    return (i,e)
    
instance Parseable CellDomain where
  parser = munge <$> many (optional parseCellName <* string " ") where
    munge xs = Set.fromList [ x | Just x <- xs ]
    
prompt s = do
  putStr s
  hFlush stdout
  getLine
    
setValueLoop w s = do
  putStrLn (pprint s)
  cellName <- prompt "Change cell: "
  cellValue <- prompt "Change value to: "
  case reads cellValue of
    [(v,"")] -> setValueLoop w (step w s (Map.fromList [(cellName,v)]))
    _ -> putStrLn "That doesn't look like a number to me!" >> setValueLoop w s
  
main = do
  a <- getArgs
  case a of
    [fileName] -> do
      s <- readFile fileName
      case parse parser fileName s of
        Left err -> print err
        Right eqs -> setValueLoop eqs Map.empty
    _ -> putStrLn "Call with one argument naming a file with a relation, a danger zone, some methods."