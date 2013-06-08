module WidgetLib where

import Data.Default
import System.Environment
import Data.Map hiding (map)
import Data.Maybe
import Data.Set (Set)
import Prelude hiding (lookup)
import SpreadsheetCommon
import SpreadsheetWidget


import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Control.Monad as M
import qualified Control.Monad.State as S
import qualified UpwardClosedSet as Rep

generateSpreadsheet :: Widget -> [Spreadsheet]
generateSpreadsheet w =
  [Spreadsheet { widget = w, locked = def, values = val } | val <- step w def def]

-- internal cells will be called xn for some integer n

getName :: Int -> String
getName x = "x" ++ show x

fresh :: S.State Int String
fresh = do
  x <- S.get
  S.put (x+1)
  return (getName x)

compileFormula :: Formula -> S.State Int (CellName,Widget)
compileFormula (Cell n)  = return (n, defaultWidget)
compileFormula (Constant c) = do
  x <- fresh
  return (x, constWidget x c)
compileFormula (BinOp o f1 f2) = do
  (a,w1) <- compileFormula f1
  (b,w2) <- compileFormula f2
  c <- fresh
  return (c, compose (compose (opWidget o c a b) w1) w2)

compileRelation :: Relation -> S.State Int Widget
compileRelation (Eq f1 f2) = do
  (a, w1) <- compileFormula f1
  (b, w2) <- compileFormula f2
  return (compose (compose (idWidget a b) w1) w2)

compile :: Relation -> Widget
compile r = --fst $ S.runState (compileRelation r) 0
  hideVars $ S.runState (compileRelation r) 0 where
    hideVars (w,0) = w
    hideVars (w,n) = let n' = n-1 in
      hide (getName n') (hideVars(w,n'))


------------------------------------------------------------------------

--------------------------------------------------------------------------

type DeterministicMethod = SpreadsheetValues -> SpreadsheetValues
deterministic :: (CellDomain -> DeterministicMethod) -> (CellDomain -> Method)
deterministic f i v = [f i v]

idMethods :: CellName -> CellName -> DangerZone -> Invariant -> CellDomain -> DeterministicMethod
idMethods aName bName dz inv i vals
  | Rep.member i dz          = vals
  | inv vals                 = vals
  | i == Set.singleton bName = Map.insert aName bVal vals
  | otherwise                = Map.insert bName aVal vals
  where
    [aVal,bVal] = map getVal [aName,bName]
    getVal name = Maybe.fromMaybe 0 $ Map.lookup name vals

idWidget a b =
  Widget {domain = dom, existentials = exs, invariant = inv, danger = dz, methods = m } where
    dom = Set.fromList [a,b]
    exs = Set.empty
    inv = \vals -> fromMaybe False $ do
      [va,vb] <- mapM (`lookup` vals) [a,b]
      return (va == vb)
    dz = Set.singleton dom
    m = deterministic $ idMethods a b dz inv

opMethods :: Op -> CellName -> CellName -> CellName ->
             DangerZone -> Invariant -> CellDomain -> DeterministicMethod
opMethods (Infix Plus)  = plusMethods
opMethods (Infix Minus) = minusMethods
opMethods (Infix Times) = timesMethods
opMethods (Infix Div)   = divMethods
opMethods (Infix Pow)   = powMethods

opWidget :: Op -> CellName -> CellName -> CellName -> Widget
opWidget o c a b =
  Widget { domain = dom, existentials = exs, invariant = inv, danger = dz, methods = m } where
    dom = Set.fromList [a,b,c]
    exs = Set.empty
    inv = \vals -> fromMaybe False $ do
      [va,vb,vc] <- mapM (`lookup` vals) [a, b, c]
      return (vc == (op o) va vb)
    dz = Set.singleton dom
    m = deterministic $ opMethods o c a b dz inv
plusWidget = opWidget (Infix Plus)
minusWidget = opWidget (Infix Minus)
timesWidget = opWidget (Infix Times)
divWidget = opWidget (Infix Div)
powWidget = opWidget (Infix Pow)

-------------------------------------------------------------

plusMethods :: CellName -> CellName -> CellName ->
               DangerZone -> Invariant -> CellDomain -> DeterministicMethod
plusMethods cName aName bName dz inv i vals
  | Rep.member i dz = vals
  | inv vals                        = vals
  | i == Set.fromList [aName,cName] = Map.insert bName bComputed vals
  | i == Set.fromList [bName,cName] = Map.insert aName aComputed vals
  | i == Set.fromList [cName]       = Map.insert aName aNew . Map.insert bName bNew $ vals
  | otherwise                       = Map.insert cName cComputed vals
  where
    [aIn,bIn,cIn] = map getVal [aName, bName, cName]
    getVal name = Maybe.fromMaybe 0 $ Map.lookup name vals
    aComputed = cIn - bIn
    bComputed = cIn - aIn
    cComputed = aIn + bIn
    aNew = if cComputed == 0 then cIn / 2 else cIn * aIn / cComputed
    bNew = if cComputed == 0 then cIn / 2 else cIn * bIn / cComputed

minusMethods :: CellName -> CellName -> CellName ->
                DangerZone -> Invariant -> CellDomain -> DeterministicMethod
minusMethods cName aName bName dz inv i vals
  | Rep.member i dz = vals
  | inv vals                        = vals
  | i == Set.fromList [aName,cName] = Map.insert bName bComputed vals
  | i == Set.fromList [bName,cName] = Map.insert aName aComputed vals
  | i == Set.fromList [cName]       = Map.insert aName aNew . Map.insert bName bNew $ vals
  | otherwise                       = Map.insert cName cComputed vals
  where
    [aIn,bIn,cIn] = map getVal [aName, bName, cName]
    getVal name = Maybe.fromMaybe 0 $ Map.lookup name vals
    aComputed = bIn + cIn
    bComputed = aIn - cIn
    cComputed = aIn - bIn
    aNew = if cComputed == 0 then cIn / 2 else cIn * aIn / cComputed
    bNew = if cComputed == 0 then cIn / 2 else cIn * bIn / cComputed

timesMethods :: CellName -> CellName -> CellName ->
                DangerZone -> Invariant -> CellDomain -> DeterministicMethod
timesMethods cName aName bName dz inv i vals
  | Rep.member i dz = vals
  | inv vals                        = vals
  | i == Set.fromList [aName,cName] = Map.insert bName bComputed vals
  | i == Set.fromList [bName,cName] = Map.insert aName aComputed vals
  | i == Set.fromList [cName]       = Map.insert aName aNew . Map.insert bName bNew $ vals
  | otherwise                       = Map.insert cName cComputed vals
  where
    [aIn,bIn,cIn] = map getVal [aName, bName, cName]
    getVal name = Maybe.fromMaybe 0 $ Map.lookup name vals
    aComputed = cIn / bIn
    bComputed = cIn / aIn
    cComputed = aIn * bIn
    sqrtC = sqrt (abs cIn)
    sqrtCComputed = sqrt (abs cComputed)
    aNew =
      if cComputed == 0 then signum cIn * sqrtC
      else signum (cIn * cComputed) * aIn * sqrtC / sqrtCComputed
    bNew =
      if cComputed == 0 then sqrtC
      else aIn * sqrtC / sqrtCComputed

divMethods :: CellName -> CellName -> CellName ->
                DangerZone -> Invariant -> CellDomain -> DeterministicMethod
divMethods cName aName bName dz inv i vals
  | Rep.member i dz                 = vals
  | inv vals                        = vals
  | i == Set.fromList [aName,cName] = Map.insert bName bComputed vals
  | i == Set.fromList [bName,cName] = Map.insert aName aComputed vals
  | i == Set.fromList [cName]       = Map.insert aName aNew . Map.insert bName bNew $ vals
  | otherwise                       = Map.insert cName cComputed vals
  where
    [aIn',bIn',cIn'] = map getVal [aName, bName, cName]
    getVal name = Map.lookup name vals
    [aIn,bIn,cIn] = map (Maybe.fromMaybe 0) [aIn',bIn',cIn']
    aComputed = bIn * cIn
    bComputed = Maybe.fromMaybe 0 $ liftM2 (/) aIn' cIn'
    cComputed = Maybe.fromMaybe 0 $ liftM2 (/) aIn' bIn'
    aNew = if bIn == 0 || cIn == 0 || cComputed == 0 then cIn
           else aIn * cIn
    bNew = if bIn == 0 || cIn == 0 || cComputed == 0 then 1
           else bIn * cComputed

powMethods :: CellName -> CellName -> CellName ->
              DangerZone -> Invariant -> CellDomain -> DeterministicMethod
powMethods cName aName bName dz inv i vals
  | Rep.member i dz = vals
  | inv vals = vals
  | i == Set.fromList [bName,cName] = Map.insert aName aComputed vals
  | i == Set.fromList [aName,cName] = Map.insert bName bComputed vals
  | i == Set.fromList [cName]       = Map.insert aName aNew . Map.insert bName bNew $ vals
  | otherwise                       = Map.insert cName cComputed vals
  where
    [aIn,bIn,cIn] = map getVal [aName,bName,cName]
    getVal name = Maybe.fromMaybe 0 $ Map.lookup name vals
    aComputed = cIn ** (1 / bIn)
    bComputed = logBase aIn cIn
    aNew = if aIn == 1 || aIn == 0 then cIn
           else aIn
    bNew = if aIn == 1 || aIn == 0 then 1
           else bComputed
    cComputed = aIn ** bIn

-------------------------------------------------------------------
----------------------------- constants ---------------------------

constWidget :: CellName -> Value -> Widget
constWidget n t = Widget
  { domain       = Set.singleton n
  , existentials = Set.empty
  , invariant    = \v -> Map.lookup n v == Just t
  , danger       = Set.singleton (Set.singleton n)
  , methods      = \_ v -> [Map.insert n t v]
  }

-------------------------------------------------------------------
------------------------------- parser ----------------------------

nubOn f = nubBy (\x y -> f x == f y)
rle f = map (\xs -> (head xs, length xs)) . groupBy (\x y -> f x == f y)

loopMany s = case rle values s of
  []       -> putStrLn "The impossible happened! Our nondeterministic widgets returned no results."
  [(s,n)]  -> putStrLn ("skipping " ++ show n ++ " duplicates") >> loop s
  ss@((s,_):_) -> do
    putStrLn "There are many possible ways to update the spreadsheet."
    forM_ (zip [1..] ss) $ \(i, (s, n)) -> do
      putStr "Choice "
      print i
      putStrLn (pprint s ++ "\n")
      when (n>1) (putStrLn ("(" ++ show n ++ " duplicates)"))
    putStrLn "Arbitrary choosing the first one."
    loop s

loop s = do
  putStrLn (pprint s)
  putStrLn ("Satisfies Invariant: " ++ (show $ invariant (widget s) (values s)) ++ "\n")
  choice <- prompt "Lock cells (l), Unlock cells (u), or change cells (c)? "
  case choice of
    "l" -> lockCells s
    "u" -> unlockCells s
    "c" -> changeCells s
    _   -> putStrLn "Please choose either (l), (u) or (c)." >> loop s

lockCells s = do
  cellNames <- words <$> prompt "Lock cells: "
  let dom = Set.fromList cellNames in
    if not $ Set.isSubsetOf dom (domain $ widget s)
    then putStrLn "We can only lock cells in the spreadsheet's domain" >> loop s
    else
      let l  = Set.union (locked s) dom in
      let s' = s {locked = l} in
      loop s'

unlockCells s = do
  cellNames <- words <$> prompt "Unlock cells: "
  let dom = Set.fromList cellNames in
    if not $ Set.isSubsetOf dom (locked s)
    then putStrLn "Not all these cells are locked!" >> loop s
    else
      let l  = Set.difference (locked s) dom in
      let s' = Spreadsheet {widget = widget s, locked = l, values = values s} in
      loop s'

changeCells s = do
  cellNames <- words <$> prompt "Change cells: "
  case cellNames of
    [] -> loopMany (stepSheet s Map.empty)
    _  ->
      let w   = widget s in
      let dom = Set.fromList cellNames in
      if not $ Set.null $ Set.intersection dom (locked s)
      then putStrLn ("That domain is locked.") >> loop s
      else
        let l = dom `Set.union` locked s in
        if Rep.member l (danger w) || (not $ Set.isSubsetOf l (domain w))
        then putStrLn ("That domain (" ++ pprint l ++ ") is dangerous!") >> loop s
        else do
          cellValues <- prompt "Change values to: "
          case readsWords cellValues of
            [] -> putStrLn "That didn't look like numbers to me!" >> loop s
            cellValues:_
              | length cellValues /= length cellNames ->
                putStrLn "Please give as many values as you gave names." >> loop s
              | otherwise -> loopMany (stepSheet s (Map.fromList (zip cellNames cellValues)))

main = do
  a <- getArgs
  case a of
    [fileName] -> do
      s <- readFile fileName
      case parse parser fileName s of
        Left err -> print err
        Right l -> -- list of relations
          let w = List.foldl g defaultWidget l where
                g w r = compose w (compile r)
          in do
            putStrLn (pprint l)
            loopMany $ generateSpreadsheet w
    _ -> putStrLn "Call with one argument naming a file with a relation."
