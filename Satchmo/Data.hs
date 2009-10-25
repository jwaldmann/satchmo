module Satchmo.Data 

( CNF, cnf, clauses
, Clause, clause, literals
, Literal, literal, nicht
)

where

import Control.Monad.State.Strict

data CNF     = CNF { clauses :: [ Clause  ] }

instance Show CNF where
    show ( CNF cs ) = unlines $ map show cs

cnf :: [ Clause ] -> CNF
cnf cs = CNF cs


data Clause  = Clause { literals :: [ Literal ] }

instance Show Clause where
    show ( Clause xs ) = unwords ( map show xs ++ [ "0" ] )

clause :: [ Literal ] -> Clause
clause ls = Clause { literals = ls }


data Literal = Literal Int 
    deriving ( Eq, Ord )

instance Show Literal where 
    show ( Literal i ) = show i

literal :: Int -> Literal
literal i | i /= 0 = Literal i


nicht :: Literal -> Literal
nicht ( Literal i ) = Literal $ negate i

