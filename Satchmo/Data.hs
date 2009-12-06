module Satchmo.Data 

( CNF, cnf, clauses
-- FIXME: exports should be abstract
, Clause(..), clause, literals
, Literal(..), literal, nicht
, Variable, variable, positive
)

where

import Control.Monad.State.Strict

newtype CNF     = CNF { clauses :: [ Clause  ] }

instance Show CNF where
    show ( CNF cs ) = unlines $ map show cs

cnf :: [ Clause ] -> CNF
cnf cs = CNF cs


newtype Clause  = Clause { literals :: [ Literal ] }

instance Show Clause where
    show ( Clause xs ) = unwords ( map show xs ++ [ "0" ] )

clause :: [ Literal ] -> Clause
clause ls = Clause { literals = ls }


newtype Literal = Literal Int
    deriving ( Eq, Ord )

instance Show Literal where 
    show ( Literal i ) = show i

instance Read Literal where
    readsPrec p = \ cs -> do
        ( i, cs') <- readsPrec p cs
        return ( Literal i , cs' )

literal :: Bool -> Variable -> Literal
literal p v | v /= 0 = 
    Literal $ if p then v else negate v


nicht :: Literal -> Literal
nicht ( Literal i ) = Literal $ negate i

-- FIXME: should be newtype
type Variable = Int

variable :: Literal -> Variable
variable ( Literal v ) = abs v

positive :: Literal -> Bool
positive ( Literal v ) = 0 < v

