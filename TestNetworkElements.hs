module TestFlow where

import Flow
import NetworkData
import NetworkRepresentation
import NetworkElements

test = let
	f1 = read ("Flow (Or [And [Eq \"Src\" (Val \"1\"),Eq \"Dst\" Any,Eq \"Port\" (Val \"R:0\")]])") :: Flow
	f2 = read ("Flow (Or [And [Eq \"Src\" (Val \"2\"),Eq \"Dst\" Any,Eq \"Port\" (Val \"R:1\")]])") :: Flow
	f3 = read ("Flow (Or [And [Eq \"Src\" Any,Eq \"Dst\" (Val \"c\"),Eq \"Port\" (Val \"P:2\")]])") :: Flow
	f4 = read ("Flow (Or [And [Eq \"Src\" (Val \"2\"),Eq \"Dst\" Any,Eq \"Port\" (Val \"F:1\")]])") :: Flow
	f5 = read ("Flow (Or [And [Eq \"Src\" (Val \"1\"),Eq \"Dst\" Any,Eq \"Port\" (Val \"P:2\")]])") :: Flow
	f6 = read ("Flow (Or [And [Eq \"Src\" Any,Eq \"Dst\" (Val \"c\"),Eq \"Port\" (Val \"R:0\")]])") :: Flow
	f7 = read ("Flow (Or [And [Eq \"Src\" Any,Eq \"Dst\" (Val \"b\"),Eq \"Port\" (Val \"R:0\")]])") :: Flow
	f8 = read ("Flow (Or [And [Eq \"Src\" (Val \"2\"),Eq \"Dst\" Any,Eq \"Port\" (Val \"P:2\")]])") :: Flow

	ff1 = read ("Flow (Or [And [Eq \"Src\" (Val \"1\"),Eq \"Dst\" Any,Eq \"Port\" (Val \"F:2\")],And [Eq \"Src\" (Val \"2\"),Eq \"Dst\" Any,Eq \"Port\" (Val \"F:2\")]])") :: Flow
	ff2 = read ("Flow (Or [And [Eq \"Src\" (Val \"1\"),Eq \"Dst\" Any,Eq \"Port\" (Val \"R:1\")],And [Eq \"Src\" (Val \"2\"),Eq \"Dst\" Any,Eq \"Port\" (Val \"R:1\")]])") :: Flow

	ff3 = read ("Flow (Or [And [Eq \"Src\" (Val \"1\"),Eq \"Dst\" (Val \"b\"),Eq \"Port\" (Val \"F:2\")],And [Eq \"Src\" (Val \"0\"),Eq \"Dst\" Any,Eq \"Port\" (Val \"P:2\")],And [Eq \"Src\" Any,Eq \"Dst\" (Val \"a\"),Eq \"Port\" (Val \"R:1\")]])") :: Flow
	ff4 = read ("Flow (Or [And [Eq \"Src\" (Val \"1\"),Eq \"Dst\" Any,Eq \"Port\" (Val \"R:1\")],And [Eq \"Src\" (Val \"2\"),Eq \"Dst\" Any,Eq \"Port\" (Val \"P:2\")]])") :: Flow
	ff5 = read ("Flow (Or [And [Eq \"Src\" Any,Eq \"Dst\" (Val \"c\"),Eq \"Port\" (Val \"F:1\")],And [Eq \"Src\" (Val \"1\"),Eq \"Dst\" (Val \"a\"),Eq \"Port\" (Val \"F:1\")]])") :: Flow
	ff6 = read ("Flow (Or [And [Eq \"Src\" (Val \"1\"),Eq \"Dst\" Any,Eq \"Port\" (Val \"P:2\")],And [Eq \"Src\" (Val \"2\"),Eq \"Dst\" Any,Eq \"Port\" (Val \"P:1\")],And [Eq \"Src\" Any,Eq \"Dst\" (Val \"c\"),Eq \"Port\" (Val \"F:1\")]])") :: Flow
	ff7 = read ("Flow (Or [And [Eq \"Src\" (Val \"1\"),Eq \"Dst\" (Val \"c\"),Eq \"Port\" (Val \"P:2\")],And [Eq \"Src\" (Val \"2\"),Eq \"Dst\" (Val \"c\"),Eq \"Port\" (Val \"P:2\")]])") :: Flow
	ff8 = read ("Flow (Or [And [Eq \"Src\" (Val \"1\"),Eq \"Dst\" (Val \"a\"),Eq \"Port\" (Val \"F:2\")],And [Eq \"Src\" (Val \"2\"),Eq \"Dst\" (Val \"a\"),Eq \"Port\" (Val \"F:2\")]])") :: Flow
	ff9 = read ("Flow (Or [And [Eq \"Src\" (Val \"1\"),Eq \"Dst\" (Val \"b\"),Eq \"Port\" (Val \"F:2\")],And [Eq \"Src\" (Val \"2\"),Eq \"Dst\" (Val \"b\"),Eq \"Port\" (Val \"F:2\")]])") :: Flow
	ff10 = read ("Flow (Or [And [Eq \"Src\" (Val \"1\"),Eq \"Dst\" (Val \"b\"),Eq \"Port\" (Val \"R:1\")],And [Eq \"Src\" (Val \"2\"),Eq \"Dst\" (Val \"b\"),Eq \"Port\" (Val \"R:1\")]])") :: Flow

	w1 = read ("Wire \"F:2\" \"R:1\"") :: Device
	w2 = read ("Wire \"R:0\" \"P:2\"") :: Device
	w3 = read ("Wire \"F:1\" \"P:0\"") :: Device
	w4 = read ("Wire \"R:1\" \"F:2\"") :: Device
	w5 = read ("Wire \"P:2\" \"R:0\"") :: Device
	w6 = read ("Wire \"P:0\" \"F:1\"") :: Device

	fw1 = read ("Firewall []") :: Device
	fw2 = read ("Firewall [(\"Src\",[\"1\",\"2\"])]") :: Device
	fw3 = read ("Firewall [(\"Dst\",[\"b\",\"a\"])]") :: Device
	fw4 = read ("Firewall [(\"Src\",[\"1\",\"3\"]),(\"Dst\",[\"a\",\"c\"])]") :: Device

	p1 = read ("Proxy (Eq \"Dst\" (Val \"c\"))") :: Device
	p2 = read ("Proxy (Eq \"Dst\" (Val \"b\"))") :: Device
	p3 = read ("Proxy (Eq \"Src\" (Val \"2\"))") :: Device

	sd1 = read ("ComplexDevice [Wire \"R:0\" \"P:2\",Proxy (Eq \"Src\" (Val \"2\"))]") :: Device
	sd2 = read ("ComplexDevice [Wire \"F:2\" \"R:1\",Firewall [(\"Src\",[\"1\",\"2\"])]]") :: Device
	sd3 = read ("ComplexDevice [Wire \"F:2\" \"R:1\",Firewall [(\"Src\",[\"1\",\"2\"])], Proxy (Eq \"Dst\" (Val \"b\"))]") :: Device
	sd4 = read ("ComplexDevice [Firewall [(\"Src\",[\"1\",\"2\"])],Proxy (Eq \"Dst\" Any), Proxy (Eq \"Src\" (Val \"2\")),Wire \"F:2\" \"R:1\"]") :: Device
	
	tests = [ 
		--wires
		fst (get_rule w2) f1,
		fst (get_rule w3) f4,
		fst (get_rule w3) ff5, 
		snd (get_rule w2) f1 == f5,
		snd (get_rule w1) ff1 == ff2,
		--firewalls
		fst (get_rule fw1) ff4,
		fst (get_rule fw2) ff6,
		fst (get_rule fw4) ff3,
		not $ fst (get_rule fw3) f6 == fst (get_rule fw4) ff3,
		snd (get_rule fw2) f3 == ff7,
		snd (get_rule fw3) ff8 == ff8,
		--proxy
		fst (get_rule p2) ff7,
		snd (get_rule p3) f5 == f8,
		snd (get_rule p2) f6 == f7,
		snd (get_rule p2) ff8 == ff9,
		snd (get_rule p1) f6 == f6,
		--complexdevice
		fst (get_rule sd1) f1,
		fst (get_rule sd2) ff1,
		fst (get_rule sd2) ff1 == (not $ fst (get_rule sd1) f2),
		fst (get_rule sd2) ff1 == (not $ fst (get_rule sd3) f2),
		fst (get_rule sd1) f1 == (not $ fst (get_rule sd1) f5),
		snd (get_rule sd1) f1 == f8,
		snd (get_rule sd2) ff1 == ff2,
		snd (get_rule sd3) ff1 == ff10,
		snd (get_rule sd4) ff3 == f2
		]
	in zip3 [1..] tests (repeat 1)
run_test = putStr $ ( foldr (\(x,y,z) acc -> "Test " ++ show x ++ "......................" ++ (if y then show z else "0") ++ "/1" ++ "\n" ++ acc) "" test) 
		++ "\nTotal......................"++(show score) ++("/25\n")
score = sum $ map (\(_, _, score) -> score) $ filter (\(_, result, _) -> result) test
