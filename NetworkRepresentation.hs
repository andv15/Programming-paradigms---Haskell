module NetworkRepresentation where

import NetworkData
import Flow

-- THE CONTENTS OF THIS FILE SHOULD NOT BE CHANGED !

--Match function type
--Should its pair Modify function be applied on this flow ?
type Match = Flow -> Bool

--Modifies the flow into a new one.
type Modify = Flow -> Flow

-- A network element is described by this pair of functions.
-- Match dictates when the element should run its logic and the Modify dictates
-- what this logic is.
type NetworkElement = (Match, Modify)

-- A network is a list of network elements.
type Network = [NetworkElement]

to_network devices = map (\x -> get_rule x ) devices

class Element a where
	get_rule :: a -> NetworkElement