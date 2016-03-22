module TestReachability where

import Flow
import NetworkData
import NetworkRepresentation
import NetworkElements
import Reachability

--  Reachability tests.
reachabilityTestLink = let
		net = [Wire "a" "b", Wire "b" "c"]
		res = reachability (to_network net) "a" "c"
	in res == ( read ("[Flow (Or [And [Eq \"Src\" Any,Eq \"Dst\" Any,Eq \"Port\" (Val \"c\")]])]") :: [Flow])

reachabilityTestDiamond = let
		net = [Wire "a" "b", Wire "b" "c", Wire "a" "d", Wire "d" "c"]
		res = reachability (to_network net) "a" "c"
	in res == ( read ("[Flow (Or [And [Eq \"Src\" Any,Eq \"Dst\" Any,Eq \"Port\" (Val \"c\")]])]") :: [Flow])

reachabilityTestLoop = let
		net = [Wire "a" "b", Wire "b" "a"]
		res = reachability (to_network net) "a" "a"
	in res == ( read ("[Flow (Or [And [Eq \"Src\" Any,Eq \"Dst\" Any,Eq \"Port\" (Val \"a\")]])]") :: [Flow])

filterTest = let
		net = [ComplexDevice [Wire "a" "b", Firewall [("Src", ["p"])]]]
		res = reachability (to_network net) "a" "b"
	in res == ( read ("[Flow (Or [And [Eq \"Src\" (Val \"p\"),Eq \"Dst\" Any,Eq \"Port\" (Val \"b\")]])]") :: [Flow])

oneRuleFilterTest = let
		net = [ComplexDevice [Wire "a" "b", Firewall [("Src", ["p", "q"])]]]
		res = reachability (to_network net) "a" "b"
	in res == ( read ("[Flow (Or [And [Eq \"Src\" (Val \"p\"),Eq \"Dst\" Any,Eq \"Port\" (Val \"b\")],And [Eq \"Src\" (Val \"q\"),Eq \"Dst\" Any,Eq \"Port\" (Val \"b\")]])]") :: [Flow])

cascadeFilterTest1 = let
		net = [	ComplexDevice [Wire "a" "b", Firewall [("Src", ["p", "q"])]],
				ComplexDevice [Wire "b" "c", Firewall [("Src", ["q"])]]]
		res = reachability (to_network net) "a" "c"
	in res == ( read ("[Flow (Or [And [Eq \"Src\" (Val \"q\"),Eq \"Dst\" Any,Eq \"Port\" (Val \"c\")]])]") :: [Flow])

cascadeFilterTest2 = let
		net = [	ComplexDevice [Wire "a" "b", Firewall [("Src", ["p", "q"])]],
				ComplexDevice [Wire "b" "c", Firewall [("Src", ["s"])]]]
		res = reachability (to_network net) "a" "c"
	in null res 

rewriteTest = let
		net = [ComplexDevice	[Wire "a" "b"], ComplexDevice [Wire "b" "c", Proxy (Eq "Src" (Val "192.0.0.1"))]]
		res = reachability (to_network net) "a" "c"
	in res == ( read ("[Flow (Or [And [Eq \"Src\" (Val \"192.0.0.1\"),Eq \"Dst\" Any,Eq \"Port\" (Val \"c\")]])]") :: [Flow])

finalTest = let
		net = [ComplexDevice [Wire "F:0" "F:1", Firewall [("Src", ["1","3"])]],
			ComplexDevice [Wire "F:2" "F:1", Firewall [("Src", ["1","3"])]],
			Wire "F:1" "P:0",
			ComplexDevice [Wire "P:0" "P:1", Proxy (Eq "Dst" (Val "2"))],
			ComplexDevice [Wire "P:0" "P:2", Proxy (Eq "Dst" (Val "2"))],
			Wire "P:2" "R:0",
			ComplexDevice [Wire "R:0" "R:1", Proxy (Eq "Dst" (Val "3")), Proxy (Eq "Src" (Val "3"))],
			Wire "R:1" "F:2"]
		res = reachability (to_network net) "F:0" "P:1"
	in res == ( read ("[Flow (Or [And [Eq \"Src\" (Val \"1\"),Eq \"Dst\" (Val \"2\"),Eq \"Port\" (Val \"P:1\")],And [Eq \"Src\" (Val \"3\"),Eq \"Dst\" (Val \"2\"),Eq \"Port\" (Val \"P:1\")]]),Flow (Or [And [Eq \"Src\" (Val \"3\"),Eq \"Dst\" (Val \"2\"),Eq \"Port\" (Val \"P:1\")]])]") :: [Flow])||
		res == (read ("[Flow (Or [And [Eq \"Src\" (Val \"3\"),Eq \"Dst\" (Val \"2\"),Eq \"Port\" (Val \"P:1\")]]),Flow (Or [And [Eq \"Src\" (Val \"1\"),Eq \"Dst\" (Val \"2\"),Eq \"Port\" (Val \"P:1\")],And [Eq \"Src\" (Val \"3\"),Eq \"Dst\" (Val \"2\"),Eq \"Port\" (Val \"P:1\")]])]") :: [Flow])

test = let
	reachabilityTests = [reachabilityTestLink, reachabilityTestDiamond, reachabilityTestLoop, filterTest, oneRuleFilterTest, cascadeFilterTest1,
		cascadeFilterTest2, rewriteTest, finalTest]
	in zip3 [1..] reachabilityTests [3,3,3,3,3,5,5,5,5]  

score = sum $ map (\(_, _, score) -> score) $ filter (\(_, result, _) -> result) test
run_test = putStr $ ( foldr (\(x,y,z) acc -> "Test " ++ show x ++ "......................" ++(if y then (show z) else show 0)  ++ "/"++ (show z )++ "\n" ++ acc) "" test) 
		++ "\nTotal......................"++(show score) ++("/35\n")