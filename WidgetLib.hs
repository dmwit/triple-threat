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

generateSpreadsheet :: Relation -> Spreadsheet
generateSpreadsheet r =
  let (n,w,l,vals) = generateSpreadsheet' 0 r in
  let sheet = Spreadsheet { widget = w, locked = l, values = vals } in
  stepSheet sheet Map.empty

-- internal cells will be called xn for some integer n
generateSpreadsheet' :: Int -> Relation -> (Int,Widget,CellDomain,SpreadsheetValues)
generateSpreadsheet' n (Eq (Cell a) (Cell b)) = 
  (n, idWidget a b, Set.empty, Map.empty)
generateSpreadsheet' n (Eq (Cell c) (BinOp o f1 f2)) =
  let bin c a b = getBinOpWidget o c a b in
  case (f1, f2) of
    (Cell a, Cell b) -> (n,bin c a b, Set.empty, Map.empty)
    (Constant v, _) ->
      let a = fresh n in
      let (n',w',l',vals') = generateSpreadsheet' (n+1) (Eq (Cell a) f2) in
      (n', w', Set.insert a l', Map.insert a v vals')
    (_, Constant v) ->  
      let b = fresh n in
      let (n',w',l',vals') = generateSpreadsheet' (n+1) (Eq f1 (Cell b)) in
      (n', w', Set.insert b l', Map.insert b v vals')
    (Cell a, _)      -> 
      let b = fresh n in
      let (n',w',l',vals') = generateSpreadsheet' (n+1) (Eq (Cell b) f2) in
      let w = hide b (compose (bin c a b) w') in
      (n',w, l', vals')
    (_, Cell b)      -> 
      let a = fresh n in
      let (n',w',l',vals') = generateSpreadsheet' (n+1) (Eq (Cell a) f1) in
      let w = hide a (compose (bin c a b) w') in 
      (n',w , l', vals')
    (_, _)           ->
      let a = fresh n in
      let (n1,w1,l1,vals1) = generateSpreadsheet' (n+1) (Eq (Cell a) f1) in
      let b = fresh n1 in
      let (n2,w2,l2,vals2) = generateSpreadsheet' (n1+1) (Eq (Cell b) f2) in
      let w = hide a (hide b (compose (bin c a b) (compose w1 w2))) in
      (n2, w, Set.union l1 l2, Map.union vals1 vals2)
generateSpreadsheet' n (Eq f (Cell c)) = generateSpreadsheet' n (Eq (Cell c) f)
generateSpreadsheet' n (Eq f1 f2) =
  let a = fresh n in
  let (n1,w1,l1,vals1) = generateSpreadsheet' (n+1) (Eq (Cell a) f1) in
  let (n2,w2,l2,vals2) = generateSpreadsheet' n1 (Eq (Cell a) f2) in
  let w = hide a (compose w1 w2) in
  (n2, w, Set.union l1 l2, Map.union vals1 vals2)
                   
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

loop s = do
  putStrLn (pprint s)
  putStrLn ("Satisfies Invariant: " ++ (pprint $ invariant (widget s) (values s)) ++ "\n")
  choice <- prompt "Lock cells (l), Unlock cells (u), or change cells (c)? "
  if choice == "l" then lockCells s
    else if choice == "u" then unlockCells s
         else if choice == "c" then changeCells s
              else putStrLn "Please choose either (l), (u) or (c)." >> loop s
                   
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
    [] -> loop (stepSheet s Map.empty)
    _  -> 
      let w   = widget s in
      let dom = Set.fromList cellNames in
      if not $ Set.null $ Set.intersection dom (locked s)
      then putStrLn ("That domain is locked.") >> loop s
      else
        let l = Set.union (locked s) $ Set.fromList cellNames in
        if dangerous (danger w) l || (not $ Set.isSubsetOf l (domain w))
        then putStrLn ("That domain (" ++ pprint l ++ ") is dangerous!") >> loop s
        else do
          cellValues <- prompt "Change values to: "
          case readsWords cellValues of
            [] -> putStrLn "That didn't look like numbers to me!" >> loop s
            cellValues:_
              | length cellValues /= length cellNames -> 
                putStrLn "Please give as many values as you gave names." >> loop s
              | otherwise -> loop (stepSheet s (Map.fromList (zip cellNames cellValues)))
                              
main = do
  a <- getArgs
  case a of 
    [fileName] -> do
      s <- readFile fileName
      case parse parser fileName s of
        Left err -> print err
        Right l -> -- list of relations
          let sheet = List.foldl g defaultSpreadsheet l where
                g sp r = composeSheet sp (generateSpreadsheet r)
          in do
            putStrLn (pprint l)
            loop sheet
    _ -> putStrLn "Call with one argument naming a file with a relation."
