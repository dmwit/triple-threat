module SpreadsheetWidget where

import SpreadsheetCommon

import qualified Data.Set as Set
import qualified Data.Map as Map

type CellDomain = Set.Set CellName
type DangerZone = Set.Set Value -> Bool
type Method     = SpreadsheetValues -> SpreadsheetValues

data Widget = Widget
  { dom :: CellDomain
  , rel :: SpreadsheetValues -> Bool
  , danger :: DangerZone
  , methods :: Data.Map CellDomain [Method]
  } deriving (Eq, Ord, Show, Read)
             

inDom :: SpreadsheetValues -> CellDomain -> Bool
inDom vals dom =
  Set.isSubsetOf (Map.keysSet vals) dom
  
restrictDom :: SpreadsheetValues -> CellDomain -> SpreadsheetValues
restrictDom vals b =
  let a    = Map.keysSet vals in
  let diff = Set.difference b a in
  Set.fold g vals diff where
    g name vals = Map.update (\val -> Nothing) name vals
              
composeMethodsOrdered :: CellDomain -> Method -> Method -> Method
composeMethodsOrdered x m1 m2 =
  let apply1 vals = overwrite vals m1 in
  let apply2 vals = overwrite vals m2 in
  apply2 . apply1
  
-- overwriteValues s1 s2 = s3 provided 
-- for all x \in dom(s1), s3(x) = s1(x)
-- for all x \in dom(s2) - dom(s1), s3(x) = s2(x)
-- for all other x, dom(s3) = Nothing
overwriteValues :: SpreadsheetValues -> SpreadsheetValues -> SpreadsheetValues
overwriteValues = undefined
    
compose :: Widget -> Widget -> Widget
compose k l =
  let dom_k = dom k in
  let dom_l = dom l in
  let dom'  = Set.union dom_k dom_l in
  let rel'  = \ vals -> 
        inDom vals dom && 
        rel k (restrictDom vals dom_k) &&
        rel l (restrictDom vals dom_l) in
  let dz_k     = danger k in
  let dz_l     = danger l in
  let danger'  = (dz_k (Set.union) dz_l) (Set.difference) (dom_k (Set.intersection) dom_l) in
  
  let in_k x = Set.intersect x dom_l in
  let in_l x = (x (Set.intersect) dom_l) (Set.union) (dom_k (Set.intersect) dom_l) in
  let methods' = undefined in
  Widget dom' rel' danger' methods'
  
        