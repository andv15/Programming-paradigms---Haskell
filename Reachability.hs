module Reachability where

import Flow
import NetworkData
import NetworkRepresentation
import NetworkElements
import Data.List

-- Computes reachability from a source to a destination.
reachability :: Network -> String -> String -> [Flow]
-- Implementation of reachability algorithm


--Verifica aplicarea regulilor retelei pe un flow
testApplyForOne :: [NetworkElement] -> Flow -> [NetworkElement]
testApplyForOne [] flow = []
testApplyForOne (l:network) flow = if ((fst l) flow) ==True then 
					(l : (testApplyForOne network flow )) 
					else 	testApplyForOne network flow

--Verifica aplicarea regulilor retelei pe toate flow-rile
testApplyForAll :: [NetworkElement] ->[Flow] ->[NetworkElement]
testApplyForAll network [] = []
testApplyForAll network (l:flows) = (testApplyForOne network l) ++ (testApplyForAll network flows) 

--Aplica regulile retelei pe un flow
applyFlow :: [NetworkElement] -> Flow -> [Flow]
applyFlow [] flow =[]
applyFlow (l:network) flow = if ((fst l) flow ) == True then 
				((((snd l) flow):(applyFlow network ((snd l) flow))))++(applyFlow network flow) 
				else 	(applyFlow network flow) 

--Aplica regulile retelei pe toate flow-rile
applyAll :: [NetworkElement] ->[Flow] -> [Flow]
applyAll network []  = []
applyAll network (f:flows) = (applyFlow network f) ++ (applyAll network flows)

--verifica daca un flow este continut sau nu intr-o lista de flow-uri
contains :: [Flow] -> Flow -> Bool
contains [] f = False
contains flows (Flow f) = if (length (filter (\(Flow x) -> if (f == x) then True else False) flows) /= 0 ) then 
				True 
				else 	False 

--elimina duplicatele dintr-o lista de flow-uri
removeDuplicates :: [Flow] -> [Flow] ->  [Flow]
removeDuplicates [] acc = acc
removeDuplicates ((Flow e):flows) acc = if ((length (filter (\(Flow x) -> if (e == x) then True else False) acc)) == 0 ) then 
						(removeDuplicates flows ((Flow e):acc)) 
						else 	(removeDuplicates flows acc)


--elimina duplicatele din intersectia aplicarii tuturor regulilor pe lista de flow-uri primite cu flow-ul general in care am suprascris portul destinatie
reachability network src dst = removeDuplicates (filter (\x -> if (NetworkData.intersect (rewrite (Eq "Port" (Val dst )) (Flow general)) x) == VoidFlow then False else True) (applyAll network [(rewrite (Eq "Port" (Val src )) (Flow general))])) []









