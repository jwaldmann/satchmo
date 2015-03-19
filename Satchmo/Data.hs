{-# language TypeFamilies #-}
{-# language GeneralizedNewtypeDeriving #-}

module Satchmo.Data 

( CNF, cnf, singleton, clauses, foldr, filter, size
, Clause, clause, literals, without
, Literal, literal, nicht, positive, variable
, Variable 
)

where

import Prelude hiding ( foldr, filter )
import qualified Prelude
  
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Data.Monoid
import Data.List ( nub )

type Variable = Int

data Literal =
     Literal { variable :: ! Variable
             , positive :: ! Bool
             }
     deriving ( Eq, Ord )

instance Show Literal where
    show l = ( if positive l then "" else "-" )
             ++ show ( variable l )

literal :: Bool -> Variable -> Literal
literal pos v  = Literal { positive = pos, variable = v }

nicht :: Literal -> Literal 
nicht x = x { positive = not $ positive x }

newtype CNF  = CNF ( S.Seq Clause )
    deriving ( Monoid )

foldr f x (CNF s) = F.foldr f x s
filter p (CNF s) = CNF $ S.filter p s

size (CNF s) = S.length s
                   
clauses (CNF s) = F.toList s

instance Show CNF  where
    show cnf = unlines $ map show $ clauses cnf

cnf :: [ Clause ] -> CNF 
cnf cs = CNF $ S.fromList cs

singleton c = CNF $ S.singleton c


newtype Clause = Clause { literals :: [ Literal ] }
   deriving ( Eq)

instance Monoid Clause where
  mappend (Clause xs)(Clause ys)= clause (xs++ys)

instance Show ( Clause ) where
  show ( Clause xs ) = unwords ( map show xs ++ [ "0" ] )

clause ::  [ Literal ] -> Clause 
clause ls = Clause { literals = nub ls }

without (Clause lits) lit =
  Clause (Prelude.filter (/= lit) lits)
