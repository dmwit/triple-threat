module ConstraintGraph where

import Control.Applicative
import Control.Arrow
import Data.Default
import Data.List as List
import Data.Map  as Map
import Data.Set  as Set
import Relation
import System.Process

data ID
	= B String
	| L ID
	| R ID
	deriving (Eq, Ord, Show, Read)

baseName (B s) = s
baseName (L id) = baseName id
baseName (R id) = baseName id

type MethodID     = ID
type ConstraintID = ID
type Variable     = String

data ConstraintGraph = ConstraintGraph
	{ constrainedVariables :: Relation ConstraintID Variable
	, methodOutputs        :: Relation MethodID     Variable
	, constraintMethods    :: Relation ConstraintID MethodID
	}

instance Default ConstraintGraph where def = ConstraintGraph def def def

baseGraph :: String -> Set Variable -> Map String (Set Variable) -> ConstraintGraph
baseGraph constraint variables methods = ConstraintGraph
	{ constrainedVariables = Relation.fromList [(B constraint, v  ) | v       <- Set.toList variables]
	, methodOutputs        = Relation.fromList [(B k         , v  ) | (k, vs) <- Map.toList methods, v <- Set.toList vs]
	, constraintMethods    = Relation.fromList [(B constraint, B k) | (k, v)  <- Map.toList methods]
	}

unionGraph :: ConstraintGraph -> ConstraintGraph -> ConstraintGraph
unionGraph (ConstraintGraph cv1 mo1 cm1) (ConstraintGraph cv2 mo2 cm2) = ConstraintGraph
	(bimapMonotonic L id cv1 `Relation.union` bimapMonotonic R id cv2)
	(bimapMonotonic L id mo1 `Relation.union` bimapMonotonic R id mo2)
	(bimapMonotonic L L  cm1 `Relation.union` bimapMonotonic R R  cm2)

(>>==) :: Ord b => Set a -> (a -> Set b) -> Set b
sa >>== f = Set.fromList (Set.toList sa >>= Set.toList . f)

ensure :: (a -> Bool) -> a -> Set a
ensure p a | p a       = Set.singleton a
           | otherwise = def

methodsFor :: ConstraintGraph -> Set Variable -> Set MethodID
methodsFor g vs =
	vs >>== \v ->
	projectR v (methodOutputs g) >>==
	ensure (\m -> projectL m (methodOutputs g) `isSubsetOf` vs)

freeVariables :: ConstraintGraph -> Set Variable
freeMethods   :: ConstraintGraph -> Set MethodID

freeMethods   g   = methodsFor g (freeVariables g)
isFree        g v = Set.size (projectR v (constrainedVariables g)) == 1
freeVariables g   = codomain (constrainedVariables g) >>== ensure (isFree g)

solve_ :: ConstraintGraph -> [[MethodID]]
solve_ g = go g (freeVariables g) where
	go g vs
		| Set.null (domain (constrainedVariables g)) = [[]]
		| otherwise = do
			m <- Set.toList (methodsFor g vs)
			c <- Set.toList (projectR m (constraintMethods g))
			let allMethods   = Set.toList . projectL c . constraintMethods    $ g
			    allVariables =              projectL c . constrainedVariables $ g
			    newGraph     = ConstraintGraph
			    	(deleteL c (constrainedVariables g))
			    	(Prelude.foldr deleteL (methodOutputs g) allMethods)
			    	(deleteL c (constraintMethods g))
			rest <- go newGraph
				((vs `Set.difference` allVariables) `Set.union`
				 Set.filter (isFree newGraph) allVariables)
			return (m:rest)

solve :: ConstraintGraph -> [([MethodID], Int)]
solve = fmap (head &&& length) . group . sort . fmap sort . solve_

-- below: sample graphs

opGraph :: String -> Variable -> Variable -> Variable -> ConstraintGraph
opGraph op a b c = baseGraph op (Set.fromList [a, b, c]) $ Map.fromList
	[ ("forward", Set.fromList [      c])
	, ("split"  , Set.fromList [a, b   ])
	, ("backL"  , Set.fromList [a      ])
	, ("backR"  , Set.fromList [   b   ])
	]

plusGraph  = opGraph "+"
timesGraph = opGraph "*"
minusGraph = opGraph "-"

node v c m = baseGraph c (Set.singleton v) (Map.singleton m (Set.singleton v))
constantGraph v c = node v (v ++ " = " ++ c) ("set to " ++ c)
inputGraph v = node v (v ++ "-input") "input"

ambiguousDiamondGraph = plusGraph "a" "b" "c" `unionGraph` plusGraph "a" "b" "d"
budgetGraph = List.foldr unionGraph def
	[ plusGraph "Student" "Equipment" "_a"
	, constantGraph "Multiplier" "0.4"
	, timesGraph "_a" "Multiplier" "Overhead"
	, plusGraph "Student" "Equipment" "_b"
	, plusGraph "_b" "Travel" "_c"
	, plusGraph "_c" "Summer" "_d"
	, plusGraph "_d" "Overhead" "Total"
	, minusGraph "Award" "Total" "Less"
	]

solutionToDot :: ConstraintGraph -> [MethodID] -> String
solutionToDot g ms = "digraph {\n" ++ intercalate ";" [methodToDot m | m <- ms] ++ "\n}" where
	methodToDot m = case Set.toList (projectR m (constraintMethods g)) of
		[c] -> methodConstraintToDot c m (projectL c (constrainedVariables g)) (projectL m (methodOutputs g))
		_   -> error $ "weird, this method is not related to exactly one constraint: " ++ show m
	methodConstraintToDot c m vs os = show (show c) ++ "[label=" ++ show (baseName c) ++ "];"
		++ intercalate ";"
			[ show (show v) ++ " -> " ++ show (show c)
			| v <- Set.toList $ vs Set.\\ os
			]
		++ intercalate ";"
			[ show (show c) ++ " -> " ++ show (show v)
			| v <- Set.toList os
			]

displaySolutions viewer g = go (fst <$> solve g) where
	go [] = return ()
	go (s:ss) = do
		writeFile "solution.dot" (solutionToDot g s)
		system "neato solution.dot -Tpng -o solution.png"
		system (viewer ++ " solution.png")
		putStrLn "q to quit, any other key to continue"
		c <- getChar
		case c of
			'q' -> return ()
			_   -> go ss
