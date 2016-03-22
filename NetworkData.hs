module NetworkData where

-- The possible values for a header field.
-- Null represents an impossible value (i.e. due to a constraint not being satisfied)
-- It is not mandatory to use Null

-- DO NOT CHANGE HOW Eq, Show and Read are implemented for this data type.
data Value = Any | Val String | Null  deriving (Show, Read, Eq)

-- A packet header will contain a fixed number of header fields which is known in advance.
-- Their type will be String. Header fields that will be used for representing packets will be: "Src", "Dst", "Port" 
-- [try to avoid using hardcoded values in flow operations' implementation]

type Header = String

-- Every header will be present in every flow bound to a value (see above).
-- This is represented by an Assignment

-- DO NOT CHANGE HOW Eq, Show and Read are implemented for this data type.
data Assignment = Eq Header Value | Null_asg deriving (Show, Read, Eq)

--TODO - define the datatype Flow

-- A Flow is represented by the following grammar:
--   <flow> ::= <constraint> | <constraint> or <flow>
--   <constraint> ::=   Src = <value> and Dst = <value> and Port = <value>
--   <value> is designated by the ADT Value, defined above

-- the above grammar is not rigid. You can design your implementation in any way you like. 
-- What is important to consider for your implementation is that each constraint explicitly 
-- specifies the values of all header-fields.

-- [hint see tests]
data Flow = Flow Or | VoidFlow deriving (Show, Read) 
data Or = Or [And] deriving (Show, Read, Eq)
data And = And [Assignment] deriving (Show, Read, Eq)

-- Type class to which Flow must be enrolled.
-- It defines the three major operations that one must be able to perform on flows.
class FlowLike a where
	intersect :: a -> a -> a
	subset :: a -> a -> Bool 
	rewrite :: Assignment -> a -> a









