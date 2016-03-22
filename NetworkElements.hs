module NetworkElements where

import NetworkRepresentation
import NetworkData
import Flow

--This file describes the language used for representing a network (also used in the tests)

-- Data type that represents the network devices

-- Wire - represents a network wire connecting two ports.
-- i.e. Wire "a" "b" -> The port "a" is connected to "b"

-- The Firewall device represents a set of filter conditions.
-- For instance [("Src", ["a"])] says the firewall is going to be 
-- traversed if and only is Src is bound to "a"
-- [(Src, ["a", "b"])] says the firewall is going to be 
-- traversed if and only if Src is bound to "a" OR "b"
-- [(Src, ["a", "b"]), (Dst, ["d"])] says the firewall is going to be 
-- traversed if and only if Src is bound to "a" OR "b" AND Dst is bound to "d"

-- The Proxy device performs a rewriting of a header value
-- Proxy (Eq "Dst" (Val "d")) - is a proxy that rewrites "Dst" value to d for every flow it processes
-- Proxy (Eq "Src" (Val "s")) - is a proxy that rewrites "Src" value to s for every flow it processes

-- The ComplexDevice is a list of Devices : Wire, Firewall and Proxy and performs 
-- the actions of the components
-- i.e. ComplexDevice[Wire "P" "R", Firewall [("Src", ["a","b"])], Proxy (Eq "Dst" (Val "d"))] -
-- the device connects ports "P" and "R", is going to be traversed if and only if Src is bound to 
-- "a" or "b" and will rewrite the "Dst" value to "d".
-- * it's assured that the rules of the components will not overlap.

--Flow-ul cel mai general
Flow general = read ("Flow (Or [And [Eq \"Src\" Any,Eq \"Dst\" Any,Eq \"Port\" Any]])") :: Flow

--And-ul cel mai general
And generalAnd = read ("And [Eq \"Src\" Any,Eq \"Dst\" Any,Eq \"Port\" Any]") :: And



--creaza un Flow cu conditiile primite
getFirewallRestrictions :: (Header,[String]) ->Flow
getFirewallRestrictions  conditions = (Flow (Or (map (\x -> doRewriteAnd  (Eq (fst conditions)  (Val x)) (And generalAnd))   $snd  conditions ))) 

--parcurge toate conditiile din lista de conditii si intersecteaza Flow-rile rezultate in urma fiecarui set de conditii
makeall::[(Header,[String])] ->Flow
makeall [] = Flow general
makeall conditions = if (length conditions) == 1 then 
				getFirewallRestrictions (head conditions) 
			else      intersect (getFirewallRestrictions (head conditions)) (getFirewallRestrictions (last conditions)) 



--verifica daca se pot aplica regulile device-urilor pe flow-ul primit
--este verificata posibilitatea aplicarii regulilor pe primul device, daca este posibila, se apelaza recursiv aplicarea 
--regulilor celorlalte device-uri in ordine, pe flow-ul modificat ("iesit" din device-ul curent), daca nu este posibila
--se returneaza "False" (nu se pot aplica toate regulile tuturor device-urilor)
canApply:: Flow -> [Device] -> Bool
canApply f devices = if ((length devices) == 1 && fst (get_rule (head devices)) f) then True
			else 	if (fst (get_rule (head devices)) f == True) then
					(canApply (snd (get_rule  (head devices)) f) (tail devices)) 
				  else False


--aplica regula primului device pe flow-ul primit si apeleaza recursiv aplicarea regulilor celorlalte device-uri in ordine
--pe flow-ul modificat ("iesit" din device)
apply::Flow -> [Device] -> Flow
apply f devices = if ((length devices) == 1) then 
			(snd (get_rule (head devices)) f) 
		   else (snd (get_rule (ComplexDevice (tail devices))) (snd (get_rule (head devices)) f)) 


data Device = Wire String String | Firewall [(Header, [String])] | Proxy Assignment | ComplexDevice [Device] deriving (Show, Read, Eq)

-- From such an device one should be able to produce a pair of match and modify rules.
instance Element Device where
	--implement get_rule for every Device constructor
	get_rule ( Wire src dst ) = (\x-> subset x (rewrite (Eq "Port" (Val src)) (Flow general)), \x -> rewrite (Eq "Port" (Val dst)) x)
	get_rule ( Firewall conditions ) = (\x-> if (intersect (makeall conditions)   x) == VoidFlow then False else True, \x -> intersect x (makeall conditions) ) 
	get_rule (Proxy (Eq h v)) = (\x-> True, \x -> rewrite (Eq h v) x)
	get_rule (ComplexDevice devices)  = (\x-> if (canApply x devices) == True then True else False, \x -> apply x devices)




