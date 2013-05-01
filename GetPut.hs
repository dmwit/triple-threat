module GetPut where
import Data.Map hiding (map)
import Data.Maybe
import Data.Set (Set)
import Prelude hiding (lookup)
import SpreadsheetCommon

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import qualified Control.Monad as M



---------------------------------------------------
---------------------put and get-------------------
            
put3 :: InOp -> Value -> Value -> Value
-- put3 o v1 v2 = v3 means v1 = (op o) v2 v3
put3 Plus  v1 v2 = v1 - v2
put3 Minus v1 v2 = v2 - v1
put3 Times v1 v2 = v1 / v2
put3 Div   v1 v2 = v2 / v1
put3 Pow   v1 v2 = logBase v2 v1

default_split :: Op -> Value -> (Value,Value)
default_split (Infix  Plus)  v_new = (v_new / 2,v_new / 2)
default_split (Infix  Minus) v_new = (v_new, 0)
default_split (Infix  Times) v_new = (sqrt (abs v_new), sqrt(abs v_new))
default_split (Infix  Div)   v_new = (v_new,1)
default_split (Infix  Pow)   v_new = (v_new,1)
default_split (Prefix Min)   v_new = (v_new,v_new)
default_split (Prefix Max)   v_new = (v_new,v_new)

invalid :: Op -> Value -> Value -> Value -> Bool
invalid (Infix Pow)  _     v1_old _ = v1_old == 0 
invalid (Prefix _)   _     _      _ = False
invalid _            v_old _      _ = v_old == 0

cell_get_maybe :: Formula -> SpreadsheetValues -> Maybe Value
cell_get_maybe (Cell new_name) values = Map.lookup new_name values
cell_get_maybe (BinOp o f1 f2) values = liftM2 (op o) (cell_get_maybe f1 values) (cell_get_maybe f2 values)

cell_get :: Formula -> SpreadsheetValues -> Value
cell_get formula values = Maybe.fromMaybe 0 $ cell_get_maybe formula values

cell_put :: Formula -> Maybe Value -> SpreadsheetValues -> SpreadsheetValues
cell_put (Cell target)   val values = Maybe.maybe (Map.delete target values) (\v -> Map.insert target v values) val
cell_put (BinOp o f1 f2) val values =
  let v1_old = cell_get_maybe f1 values in
  let v2_old = cell_get_maybe f2 values in
  let (v1_new,v2_new) = Maybe.maybe (Nothing,Nothing) fun val where
        fun v = lift_pair (split_op o v v1_old v2_old)
        lift_pair (x,y) = (Just x,Just y)
  in 
  cell_put f2 v2_new (cell_put f1 v1_new values)
  

split_op :: Op -> Value -> Maybe Value -> Maybe Value -> (Value,Value)
split_op o v_new mv1_old mv2_old = 
  let v_old = liftM2 (op o) mv1_old mv2_old in
  case v_old of
    Nothing    -> default_split o v_new
    Just v_old -> 
      --since v_old is defined, v1_old and v2_old are defined
      let v1_old = Maybe.fromMaybe 0 mv1_old in
      let v2_old = Maybe.fromMaybe 0 mv2_old in
      if invalid o v_old v1_old v2_old then 
        default_split o v_new 
      else
        case o of
          Infix Plus -> 
            let scale = v_new / v_old in
            let v1 = scale * v1_old in
            let v2 = put3 Plus v_new v1 in
            (v1, v2) 
          Infix Minus ->
            let scale = v_new / v_old in
            let v1 = scale * v1_old in
            let v2 = put3 Minus v_new v1 in
            (v1, v2)
          Infix Times ->
            let scale = sqrt (abs (v_new / v_old)) in
            let v1 = scale * v1_old in
            let v2 = put3 Times v_new v1 in
            (v1, v2)
          Infix Div ->
            let scale = sqrt (abs (v_new / v_old)) in
            let v1 = scale * v1_old in
            let v2 = put3 Div v_new v1 in 
            (v1,v2)
          Infix Pow ->
            let v2 = put3 Pow v_new v1_old in
            (v1_old, v2)
          Prefix Min ->
            let v1 = max v1_old v_new in
            let v2 = max v2_old v_new in
            if v1 == v2 then
              (v_new,v_new) 
            else if v1 < v2 then
                   (v_new,v2) 
                 else
                   (v1,v_new)
          Prefix Max ->
            let v1 = min v1_old v_new in
            let v2 = min v2_old v_new in
            if v1 == v2 then
              (v_new,v_new)
            else if v1 > v2 then
                   (v_new,v2)
                 else
                   (v1,v_new)
