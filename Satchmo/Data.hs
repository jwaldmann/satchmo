{-# language TypeFamilies #-}

module Satchmo.Data 

( CNF, cnf, clauses
-- FIXME: exports should be abstract
, Clause(..), clause, literals
, Literal (..), nicht, positive, variable
, Variable 
)

where

import Control.Monad.State.Strict

type Variable = Int

type Literal =  Int -- variable multiplied by polarity


literal :: Bool -> Variable -> Literal
literal pos v  | v > 0  = if pos then v else negate v 

nicht :: Literal -> Literal 
nicht x = negate x

positive :: Literal -> Bool
positive x = x > 0

variable :: Literal -> Variable
variable l = abs l

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


