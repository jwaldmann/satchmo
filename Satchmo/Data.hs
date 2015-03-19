{-# language TypeFamilies #-}

module Satchmo.Data 

( CNF, cnf, clauses
, Clause, clause, literals
, Literal, literal, nicht, positive, variable
, Variable 
)

where

import qualified Data.Sequence as S
import Data.Foldable (toList)
import Data.Monoid


type Variable = Int

data Literal =
     Literal { variable :: ! Variable
             , positive :: ! Bool
             }

instance Show Literal where
    show l = ( if positive l then "" else "-" )
             ++ show ( variable l )

literal :: Bool -> Variable -> Literal
literal pos v  = Literal { positive = pos, variable = v }

nicht :: Literal -> Literal 
nicht x = x { positive = not $ positive x }

newtype CNF  = CNF ( S.Seq Clause )
clauses (CNF s) = Data.Foldable.toList s

instance Show CNF  where
    show cnf = unlines $ map show $ clauses cnf

cnf :: [ Clause ] -> CNF 
cnf cs = CNF $ S.fromList cs

instance Monoid CNF where
  mempty = CNF S.empty
  mappend (CNF c1) (CNF c2) = CNF $ mappend c1 c2

newtype Clause = Clause { literals :: [ Literal ] }

instance Show ( Clause ) where
    show ( Clause xs ) = unwords ( map show xs ++ [ "0" ] )

clause ::  [ Literal ] -> Clause 
clause ls = Clause { literals = ls }


