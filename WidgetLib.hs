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
  let (_,w) = generateWidget' 0 r in
  w

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
                 

data NumOperator = NumOp
  { binop :: Value -> Value -> Value
  , negop :: Value -> Value -> Value
    -- scale takes a value, a new and an old value of the updated elt
  , scalea :: Value -> Value -> Value -> Value
  , scaleb :: Value -> Value -> Value -> Value
  }

numOpMethods :: NumOperator -> CellName -> CellName -> CellName ->
             DangerZone -> CellDomain -> Method
numOpMethods (NumOp { binop = bop, negop = nop, scalea = sa, scaleb = sb})
          cName aName bName dz i vals
  | dangerous dz i = vals -- dz == [a,b,c]
  | i == Set.fromList [aName,cName] = Map.insert bName bComputed vals
  | i == Set.fromList [bName,cName] = Map.insert aName aComputed vals
  | i == Set.fromList [cName]       = Map.insert aName aNew . Map.insert bName bNew $ vals
  | otherwise                       = Map.insert cName cComputed vals
  where
    aIn = Maybe.fromMaybe 0 (Map.lookup aName vals)
    bIn = Maybe.fromMaybe 0 (Map.lookup bName vals)
    cIn = Maybe.fromMaybe 0 (Map.lookup cName vals) 
    aComputed = nop cIn bIn
    bComputed = nop cIn aIn
    cComputed = bop aIn bIn
    aNew = sa aIn cComputed cIn
    bNew = sb bIn cComputed cIn

-- c = a op b
numOpWidget :: NumOperator -> CellName -> CellName -> CellName -> Widget
numOpWidget o c a b = answer where
  answer = Widget
    { domain    = Set.fromList [a,b,c]
    , invariant = \vals -> fromMaybe False $ do
        [va, vb, vc] <- mapM (`lookup` vals) [a, b, c]
        return (binop o va vb == vc)
    , danger    = Set.singleton (domain answer)
    , methods   = numOpMethods o c a b (danger answer)
    }

-- meant for + and -
scaleLin :: Value -> Value -> Value -> Value -> Value
-- 'def' input is the default case
scaleLin val old new def = if old /= 0 then val * new / old else def

plusOperator :: NumOperator
plusOperator = NumOp (+) (-) s s where
  -- default: c = c/2 + c/2
  s val old new = scaleLin val old new (new / 2)

-- c = a + b
plusWidget :: CellName -> CellName -> CellName -> Widget
plusWidget = numOpWidget plusOperator

minusOperator :: NumOperator
minusOperator = NumOp (-) (+) sa sb where
  -- default: c = c - 0
  sa val old new = scaleLin val old new new
  sb val old new = scaleLin val old new 0

-- c = a - b
minusWidget :: CellName -> CellName -> CellName -> Widget
minusWidget = numOpWidget minusOperator


timesOperator = NumOp (*) (/) sa sb where
  sa val old new =
    if old == 0 then sqrt( abs( new ) ) else
      val * sqrt( abs( new / old ) )
  sb val old new =
    if old == 0 then sqrt( abs( new ) ) else
      if new / old < 0 then (-val) * sqrt( abs( new / old ) ) else
        val * sqrt( abs( new / old ) )
-- c = a * b
timesWidget = numOpWidget timesOperator

divOperator = NumOp (/) (flip (/)) sa sb where
  sa val old new = val * new
  sb val old new = val * old
-- c = a / b
divWidget = numOpWidget divOperator



-------------------------------------------------------------------
------------------------------- parser ----------------------------

loop w s = do
  putStrLn (pprint s)
  putStrLn ("Satisfies Invariant: " ++ (pprint $ invariant w s) ++ "\n")
  cellNames <- words <$> prompt "Change cells: "
  case cellNames of
    [] -> loop w (step w s Map.empty)
    _  -> 
      if dangerous (danger w) (Set.fromList cellNames)
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
