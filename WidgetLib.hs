module WidgetLib where

--import Data.Default
import Data.Map hiding (map)
import Data.Maybe
import Data.Set (Set)
import Prelude hiding (lookup)
import SpreadsheetCommon
import SpreadsheetWidget

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Control.Monad as M

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
    [aIn,bIn,cIn] = map (vals !) [aName, bName, cName]
    aComputed = nop cIn bIn
    bComputed = nop cIn aIn
    cComputed = bop aIn bIn
    aNew = sa aIn cComputed cIn
    bNew = sb bIn cComputed cIn

-- c = a op b
numOpWidget :: NumOperator -> CellName -> CellName -> CellName -> GenWidget
numOpWidget o c a b = answer where
  answer = GenWidget
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
plusWidget :: CellName -> CellName -> CellName -> GenWidget
plusWidget = numOpWidget plusOperator

minusOperator :: NumOperator
minusOperator = NumOp (-) (+) sa sb where
  -- default: c = c - 0
  sa val old new = scaleLin val old new new
  sb val old new = scaleLin val old new 0

-- c = a - b
minusWidget :: CellName -> CellName -> CellName -> GenWidget
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

idWidget a b = GenWidget
  { domain    = Set.fromList [a, b]
  , invariant = \vals -> vals ! a == vals ! b
  , danger    = Set.fromList [Set.fromList [a, b]]
  , methods   = \i vals -> if i == Set.fromList [a] then Map.insert b (vals ! a) vals else Map.insert a (vals ! b) vals
  }


