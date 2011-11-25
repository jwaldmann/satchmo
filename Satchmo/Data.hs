{-# language TypeFamilies #-}

module Satchmo.Data 

( CNF, cnf, clauses
-- FIXME: exports should be abstract
, Clause(..), clause, literals
, Literal (..), literal, nicht, positive, variable
, Variable 
)

where

import Control.Monad.State.Strict

type Variable = Int

data Literal = Literal { variable :: Variable , positive :: Bool }

instance Show Literal where
    show l = ( if positive l then "" else "-" ) ++ show ( variable l )

literal :: Bool -> Variable -> Literal
literal pos v  = Literal { positive = pos, variable = v }

nicht :: Literal -> Literal 
nicht x = x { positive = not $ positive x }

newtype CNF     = CNF { clauses :: [ Clause ] }

instance Show ( CNF  ) where
    show ( CNF cs ) = unlines $ map show cs

cnf :: [ Clause ] -> CNF 
cnf cs = CNF cs


newtype Clause = Clause { literals :: [ Literal ] }

instance Show ( Clause ) where
    show ( Clause xs ) = unwords ( map show xs ++ [ "0" ] )

clause ::  [ Literal ] -> Clause 
clause ls = Clause { literals = ls }


