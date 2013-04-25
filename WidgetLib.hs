module WidgetLib where

import SpreadsheetCommon
import SpreadsheetWidget

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Control.Monad as M

emptyMethod :: Method
emptyMethod vals = Map.emtpy

look = Map.lookup 

instance Eq Set.Set where
  x == y = Set.isSubsetOf x y && Set.isSubsetOf y x

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
          c a b dz i =
  | dangerous dz i -> emptyMethod -- dz == [a,b,c]
  | i == Set.fromList [a,c] ->
    Map.update (\ _ -> M.liftM2 nop (look vals c) (look vals a)) b vals
  | i == Set.fromList [b,c] ->
    Map.update (\ _ -> M.liftM2 nop (look vals c) (look vals b)) a vals
  | i == Set.singleton a    ->
    Map.update fb b (Map.update fa a vals) where
      fa val = M.liftM (sa val) vc (look vals c)
      fb val = M.liftM (sb val) vc (look vals c)
      vc = M.liftM bop (look vals a) (look vals b)
  | otherwise               -> -- i == [a],[b],[a,b],[]
    Map.update (\ _ -> M.liftM2 bop (look vals a) (look vals b)) c vals

-- c = a op b
numOpWidget :: NumOperator -> CellName -> CellName -> CellName -> Widget
numOpWidget o c a b =
  Widget { dom = dom, invariant = inv, danger = dz, methods = f } where
    dom = Set.fromList [a,b,c]
    inv = \ vals -> Maybe.fromMaybe False $ M.liftM2 (==) 
                    (M.liftM2 (binop o) (look vals a) (look vals b))
                    (look vals c) 
    dz = Set.singleton $ dom
    f i = numOpMethods o c a b dz i
    
    
-- meant for + and -
scaleLin :: Value -> Value -> Value -> Value -> Value
-- 'def' input is the default case
scaleLin val old new def = if old /= 0 then val * new / old else def

plusOperator :: NumOperator 
plusOperator = { (+), (-), s, s } where
  -- default: c = c/2 + c/2
  s val old new = scaleLin val old new (new / 2)

-- c = a + b
plusWidget :: CellName -> CellName -> CellName -> Widget
plusWidget = numOpWidget plusOperator
    

minusOperator :: NumOperator
minusOperator = { (-), (+), sa, sb } where
  -- default: c = c - 0
  sa val old new = scaleLin val old new new
  sb val old new = scaleLin val old new 0

-- c = a - b
minusWidget :: CellName -> CellName -> CellName -> Widget
minusWidget = numOpWidget minusOperator


timesOperator = { (*), (/), sa, sb } where
  sa val old new = 
    if old == 0 then sqrt( abs( new ) ) else 
      val * sqrt( abs( new / old ) )
  sb val old new =
    if old == 0 then sqrt( abs( new ) ) else
      if new / old < 0 then (-val) * sqrt( abs( new / old ) ) else
        val * sqrt( abs( new / old ) )
-- c = a * b
timesWidget = numOpWidget timesOperator

divOperator = { (/), (/) . swap, sa, sb } where
  swap (a,b) = (b,a) 
  sa val old new = val * new
  sb val old new = val * old
-- c = a / b
divWidget = numOpWidget divOperator
