module WidgetLib where

--import Data.Default
import System.Environment
import Data.Map hiding (map)
import Data.Maybe
import Data.Set (Set)
import Prelude hiding (lookup)
import SpreadsheetCommon
import SpreadsheetWidget hiding (main)


import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Control.Monad as M

-- getBinOpWidget o a b c <=> a o b c
getBinOpWidget :: Op -> CellName -> CellName -> CellName -> Widget
getBinOpWidget (Infix Plus)  = plusWidget
getBinOpWidget (Infix Minus) = minusWidget
getBinOpWidget (Infix Times) = timesWidget
getBinOpWidget (Infix Div)   = divWidget

fresh :: Int -> CellName
fresh n = "x" ++ show n

generateWidget :: Relation -> Widget
generateWidget r =
  let (n,w) = generateWidget' 0 r in
  hidevars n w where
    hidevars 0 w = hide (fresh 0) w
    hidevars n w = hide (fresh n) $ hidevars (n-1) w

-- internal cells will be called xn for some integer n
generateWidget' :: Int -> Relation -> (Int,Widget)
generateWidget' n (Eq (Cell a) (Cell b)) = (n,idWidget a b)
generateWidget' n (Eq (Cell c) (BinOp o f1 f2)) =
  let w c a b = getBinOpWidget o c a b in
  case (f1, f2) of
    (Cell a, Cell b) -> (n,w c a b)
    (Cell a, _)      -> 
      let b = fresh n in
      let (n',w') = generateWidget' (n+1) (Eq (Cell b) f2) in
      (n',compose (w c a b) w')
    (_, Cell b)      -> 
      let a = fresh n in
      let (n',w') = generateWidget' (n+1) (Eq (Cell a) f1) in
      (n',compose (w c a b) w')
    (_, _)           ->
      let a = fresh n in
      let (n1,w1) = generateWidget' (n+1) (Eq (Cell a) f1) in
      let b = fresh n1 in
      let (n2,w2) = generateWidget' (n1+1) (Eq (Cell b) f2) in
      (n2, compose (w c a b) (compose w1 w2))
generateWidget' n (Eq f (Cell c)) = generateWidget' n (Eq (Cell c) f)
generateWidget' n (Eq f1 f2) =
  let a = fresh n in
  let (n1,w1) = generateWidget' (n+1) (Eq (Cell a) f1) in
  let (n2,w2) = generateWidget' n1 (Eq (Cell a) f2) in
  (n2, compose w1 w2)
                   
------------------------------------------------------------------------
                  
--------------------------------------------------------------------------

idWidget a b = 
  getWidget $ RelationWidget {relation = rel, equations = eq, dangerZone = dz}
  where
    rel = Eq (Cell a) (Cell b)
    dz  = Set.singleton $ Set.fromList [a,b]
    eq0 = Equation a (Cell b)
    eqa = Equation b (Cell a) 
    eq  = Map.fromList [(Set.empty, Set.singleton eq0),
                        (Set.fromList [a], Set.singleton eqa),
                        (Set.fromList [b], Set.singleton eq0)]
           

opMethods :: Op -> CellName -> CellName -> CellName -> 
             DangerZone -> Invariant -> CellDomain -> Method
opMethods (Infix Plus) = plusMethods
opMethods (Infix Minus) = minusMethods
opMethods (Infix Times) = timesMethods
opMethods (Infix Div) = divMethods
    
opWidget :: Op -> CellName -> CellName -> CellName -> Widget
opWidget o c a b =
  Widget { domain = dom, existentials = exs, invariant = inv, danger = dz, methods = m } where
    dom = Set.fromList [a,b,c]
    exs = Set.empty
    inv = \vals -> fromMaybe False $ do
      [va,vb,vc] <- mapM (`lookup` vals) [a, b, c]
      return (vc == (op o) va vb)
    dz = Set.singleton dom
    m = opMethods o c a b dz inv
    
plusWidget = opWidget (Infix Plus) 
minusWidget = opWidget (Infix Minus)
timesWidget = opWidget (Infix Times)
divWidget = opWidget (Infix Div)

-------------------------------------------------------------

plusMethods :: CellName -> CellName -> CellName -> 
               DangerZone -> Invariant -> CellDomain -> Method
plusMethods cName aName bName dz inv i vals
  | dangerous dz i = vals
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
                DangerZone -> Invariant -> CellDomain -> Method
minusMethods cName aName bName dz inv i vals
  | dangerous dz i = vals
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
                DangerZone -> Invariant -> CellDomain -> Method
timesMethods cName aName bName dz inv i vals
  | dangerous dz i = vals
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
                DangerZone -> Invariant -> CellDomain -> Method
divMethods cName aName bName dz inv i vals
  | dangerous dz i                  = vals
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




-------------------------------------------------------------------
------------------------------- parser ----------------------------

loop w s = do
  putStrLn (pprint (domain w,s))
  putStrLn ("Satisfies Invariant: " ++ (pprint $ invariant w s) ++ "\n")
  cellNames <- words <$> prompt "Change cells: "
  case cellNames of
    [] -> loop w (step w s Map.empty)
    _  -> 
      let dom = Set.fromList cellNames in
      if dangerous (danger w) dom || (not $ Set.isSubsetOf dom (domain w))
      then putStrLn "That domain is dangerous!" >> loop w s
      else do
           cellValues <- prompt "Change values to: "
           case readsWords cellValues of
             [] -> putStrLn "That didn't look like numbers to me!" >> loop w s
             cellValues:_
               | length cellValues /= length cellNames -> 
                 putStrLn "Please give as many values as you gave names." >> loop w s
               | otherwise -> loop w (step w s (Map.fromList (zip cellNames cellValues)))

main = do
  a <- getArgs
  case a of 
    [fileName] -> do
      s <- readFile fileName
      case parse parser fileName s of
        Left err -> print err
        Right l -> -- list of relations
          let w = List.foldl g defaultWidget l where
                g w r = compose w (generateWidget r)
          in do
            putStrLn (pprint l)
            loop w Map.empty
    _ -> putStrLn "Call with one argument naming a file with a relation."
