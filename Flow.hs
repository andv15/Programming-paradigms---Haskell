module Flow where

import NetworkData

--FUNCTII AJUTATOARE

--rescrie valoarea pentru Assignment-ul primit daca cele doua headere corespund
doRewriteAsg :: Assignment -> Assignment -> Assignment
doRewriteAsg a Null_asg = Null_asg
doRewriteAsg (Eq hh vv) (Eq h v) = if (hh == h) then (Eq hh vv) else (Eq h v)

--rescrie Assignment-ul primit pentru fiecare element din lista de Assignment-uri primita (And)
doRewriteAnd::Assignment -> And -> And
doRewriteAnd asg (And []) = (And [])
doRewriteAnd asg (And and) = And (map (\x -> (doRewriteAsg asg x)) and )




--returneaza intersectia a doua Assignment-uri astfel:
-- Any cu X   =>  X
-- X cu Any => X
-- X cu Y => X daca X == Y altfel Null_asg
intersectAsg:: Assignment -> Assignment -> Assignment
intersectAsg asg Null_asg = Null_asg
intersectAsg Null_asg asg = Null_asg
intersectAsg (Eq h1 v1) (Eq h2 v2) = if (h1 == h2 && v1 == v2) then (Eq h1 v1)
				     	else (if (h1 == h2 && v1 == Any) then (Eq h2 v2)
					  	else (if (h1 == h2 && v2 == Any) then (Eq h1 v1)
							else Null_asg))

--returneaza un And ce reprezinta intersectia And-urilor primite
--intersecteaza pe rand ,in paralel cele doua Assignment-uri, daca intersectia e diferita de Null_asg este introdusa in lista ce va fi returnata
intersectAnd::And -> And -> [Assignment]
intersectAnd (And []) (And []) = []
intersectAnd and (And []) = []
intersectAnd (And []) and = []
intersectAnd (And and1) (And and2) = if ( (intersectAsg (head and1) (head and2)) /= Null_asg ) then 
						((intersectAsg (head and1) (head and2)):(intersectAnd (And (tail and1)) (And (tail and2))))
					else (intersectAnd (And (tail and1)) (And (tail and2)))

--returneaza intersectia primului element cu fiecare element din lista a doua(Un And ce contine 3 Assigment diferite de Null_asg)
interFlow:: And ->[And] -> [And]
interFlow and [] = [(And [])]
interFlow (And []) and = [(And [])]
interFlow and ( e:l ) = if ( length (intersectAnd and e) == 3) then  
				(And (intersectAnd and e)):(interFlow and l) 
				else	(interFlow and l)  

--elimina elementele "duplicate"(care sunt subset-uri ale altor elemente) in lista de And-uri pe care o returneaza
--returneaza lista de And-uri a intersectiei. Se intereaza prin fiecare And al primei liste si se realizeaza intersectia 
--cu ce-a dea doua lista
interFlowmake:: [And] -> [And] ->[And] 
interFlowmake and1 and2 = filter (\x -> if(findMorePermisive x (filter (/= x) (concat (map (\x-> (interFlow x and2) ) and1))) == False) then True else False) (concat (map (\x-> (interFlow x and2) ) and1))


--verifica daca primul Assignment e subset al celui de-al doilea (al doilea are valoare egala sau Any)
isSubsetAsg:: Assignment -> Assignment -> Bool
isSubsetAsg _ Null_asg = False
isSubsetAsg (Eq h1 v1) (Eq h2 v2) = if ( h1 == h2 && (v1 == v2 || v2 == Any)) then True else False 


--verifica pentru fiecare Assignment din cele doua liste (And-uri) de pe aceleasi pozitii, daca cel din prima lista este subset 
--al celui din a doua lista 
isSubsetAnd::And -> And -> Bool
isSubsetAnd (And []) (And []) = True
isSubsetAnd and1 (And []) = False
isSubsetAnd (And []) (And and2) = False
isSubsetAnd (And and1) (And and2) = if ( (isSubsetAsg (head and1) (head and2)) == True) then 
					isSubsetAnd (And (tail and1)) (And (tail and2))
				     else False

--verifica daca exista in lista de And-uri primita un And mai "permisiv/general" decat cel primit
findMorePermisive :: And ->[And] ->Bool
findMorePermisive and [] = False
findMorePermisive and (e:l) = if(isSubsetAnd and e )/=False then True else (findMorePermisive and l)

--verifica pentru fiecare And din prima lista daca exista un And mai "permisiv/general" in cea de-a doua
subFlowmake :: [And] -> [And] ->[Bool]
subFlowmake and1 and2 = ( map (\x -> (findMorePermisive x and2) ) and1)

instance FlowLike Flow where 
	
	-- Intersection of two constraints should be performed for each pair of corresponding headers.
	-- For example for two simple Flows: 
	-- i.e. Flow $ Or $ [And [Eq "Src" (Val "A"), Eq "Dst" ANY, Eq "Port" ANY]] intersect
	--      Flow $ Or $ [And [Eq "Src" ANY, Eq "Dst" (Val "B"), Eq "Port" ANY]] ->
	--      Flow $ Or $ [And [Eq "Src" "A", Eq "Dst" (Val "B"), Eq "Port" ANY]]

	-- where expr $ expr' will be interpreted as expr (expr'). In words, $ opens a parenthesis where it occurs and 
	-- closes it at the end of the line

	-- Intersection with the VoidFlow is VoidFlow
	-- Atention! Intersection of Flows works the same as the intersection of math sets 
	-- [ here the sets would be the sets containing the constraints corresponding to every flow ]
	-- Check this out if confused: http://en.wikipedia.org/wiki/Union_(set_theory)#Union_and_intersection
	-- INTERSECT function
	intersect a VoidFlow = VoidFlow
	intersect VoidFlow a = VoidFlow
	intersect (Flow(Or f1)) (Flow (Or f2)) = if filter (\x -> x /= (And []) ) (interFlowmake f1 f2) /= [] then 
							Flow( Or( filter (\x->x/= (And []) ) (interFlowmake f1 f2))) 
						else	VoidFlow


	-- Check if Flow a is subset of flow b
	-- VoidFlow is subset of any Flow
	-- Nothing is subset of VoidFlow (except from VoidFlow)

	-- A Flow is a subset of another if all its constraints have a corresponding constraint in the
	-- second flow to which they are a subset of.

	-- The subset check for two constraints works as in the case of intersect - the check
	-- should be applied in sequence for every corresponding header pairs in the two constraints

	-- check TestFlow - ff3 is subset of ff5 - because for every constraint from ff3 ( first and second )
	-- you can find another constraint in the second flow ff5:
	-- the first constraint of ff3 is subset of the second constraint of ff5
	-- and the second constraint of ff3 is subset of the first constraint of ff5

	-- SUBSET function
	subset VoidFlow VoidFlow = True 
	subset _ VoidFlow = False
	subset VoidFlow _ = True
	subset (Flow (Or f1)) (Flow (Or f2)) = if filter(\x -> x == False) (subFlowmake f1 f2) == [] then True else False


	-- Rewrite a header value to all components of Flow
	-- This does nothing to the VoidFlow.
	-- REWRITE function
	rewrite _ VoidFlow = VoidFlow
	rewrite Null_asg f = VoidFlow
	rewrite asg (Flow (Or f)) = Flow (Or (map (\x -> (doRewriteAnd asg x) ) f ))


-- Flow as instance of Eq
-- Two flow are equal when they represent the same set of constraints.
-- Hint: You should implement this after the above functions were implemented and use them.
instance (Eq Flow) where
	VoidFlow == VoidFlow = True
	VoidFlow == (Flow _) = False
	(Flow _) == VoidFlow = False
	b == a = (subset a b) && (subset b a)

