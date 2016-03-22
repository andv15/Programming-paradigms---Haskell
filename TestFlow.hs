module TestFlow where

import Flow
import NetworkData

test = let
	f1 = read ("Flow (Or [And [Eq \"Src\" (Val \"a\"),Eq \"Dst\" Any,Eq \"Port\" Any]])") :: Flow
	f2 = read ("Flow (Or [And [Eq \"Src\" (Val \"b\"),Eq \"Dst\" Any,Eq \"Port\" Any]])") :: Flow
	f3 = read ("Flow (Or [And [Eq \"Src\" Any,Eq \"Dst\" (Val \"c\"),Eq \"Port\" Any]])") :: Flow

	ff2 = read ("Flow (Or [And [Eq \"Src\" (Val \"a\"),Eq \"Dst\" Any,Eq \"Port\" Any],And [Eq \"Src\" (Val \"b\"),Eq \"Dst\" Any,Eq \"Port\" Any]])") :: Flow
	ff3 = read ("Flow (Or [And [Eq \"Src\" Any,Eq \"Dst\" (Val \"c\"),Eq \"Port\" Any],And [Eq \"Src\" (Val \"a\"),Eq \"Dst\" Any,Eq \"Port\" Any]])") :: Flow
	ff4 = read ("Flow (Or [And [Eq \"Src\" (Val \"a\"),Eq \"Dst\" Any,Eq \"Port\" Any],And [Eq \"Src\" (Val \"b\"),Eq \"Dst\" Any,Eq \"Port\" Any],And [Eq \"Src\" Any,Eq \"Dst\" (Val \"c\"),Eq \"Port\" Any]])") :: Flow
	ff5 = read ("Flow (Or [And [Eq \"Src\" (Val \"a\"),Eq \"Dst\" Any,Eq \"Port\" Any],And [Eq \"Src\" Any,Eq \"Dst\" (Val \"c\"),Eq \"Port\" Any],And [Eq \"Src\" (Val \"e\"),Eq \"Dst\" (Val \"d\"),Eq \"Port\" Any]])") :: Flow
	
	ff6 = read ("Flow (Or [And [Eq \"Src\" (Val \"a\"),Eq \"Dst\" Any,Eq \"Port\" (Val \"P\")],And [Eq \"Src\" (Val \"b\"),Eq \"Dst\" Any,Eq \"Port\" (Val \"P\")]])") :: Flow
	ff7 = read ("Flow (Or [And [Eq \"Src\" Any,Eq \"Dst\" Any,Eq \"Port\" Any],And [Eq \"Src\" Any,Eq \"Dst\" (Val \"c\"),Eq \"Port\" Any]])") :: Flow
	mostGeneralF = read ("Flow (Or [And [Eq \"Src\" Any,Eq \"Dst\" Any,Eq \"Port\" Any]])" ) :: Flow

	tests = [ 
		subset f1 ff2,
		subset f1 ff3,
		subset f1 mostGeneralF,
		subset f1 f1,
		not $ subset f1 f2,
		subset ff2 ff4,
		subset ff3 ff4,
		subset f3 ff3,
		subset ff3 ff5,
		subset ff3 mostGeneralF,
		not $ subset ff2 f1,
		not $ subset ff3 f1,
		not $ subset mostGeneralF ff3,
		subset f1 $ intersect ff3 ff2,
		subset f2 $ intersect ff4 ff2,   --15
		subset f3 $ intersect ff4 ff3,
		not $ subset f2 $ intersect ff3 ff2,
		not $ subset f3 $ intersect ff3 ff2,
		mostGeneralF == mostGeneralF,
		not $ f1 == ff2,			
		not $ f1 == ff3,
		not $ f1 == ff4,
		not $ f1 == mostGeneralF,		
		not $ ff3 == ff2,
		not $ ff2 == ff4,
		not $ ff3 == ff4,
		intersect f1 ff2 == f1,
		intersect ff3 mostGeneralF == ff3,
		intersect mostGeneralF mostGeneralF == mostGeneralF,
		intersect mostGeneralF VoidFlow == VoidFlow,
		not $ intersect ff3 ff2 == f2,
		not $ intersect ff3 ff2 == f3,
		subset ff3 $ intersect ff3 mostGeneralF,
		not $ subset ff3 $ intersect ff2 mostGeneralF,
		subset VoidFlow f1,
		rewrite (Eq "Port" (Val "P")) VoidFlow == VoidFlow,
		rewrite (Eq "Src" (Val "b")) f1 == f2,
		rewrite (Eq "Port" (Val "P")) ff2 == ff6,
		rewrite (Eq "Src" Any) ff4 == ff7,
		rewrite (Eq "Src" Any) mostGeneralF == mostGeneralF					
		]
	in zip3 [1..] tests (repeat 1)
run_test = putStr $ ( foldr (\(x,y,z) acc -> "Test " ++ show x ++ "......................" ++ (if y then show z else show 0) ++ "/1" ++ "\n" ++ acc) "" test) 
		++ "\nTotal......................"++(show score) ++("/40\n")
score = sum $ map (\(_, _, score) -> score) $ filter (\(_, result, _) -> result) test
