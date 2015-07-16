-- | this module just defines types for formulas,
-- it is not meant to contain efficient implementations
-- for formula manipulation.

{-# language TypeFamilies #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language TemplateHaskell #-}

module Satchmo.Data 

( CNF, cnf, clauses, size
, Clause, clause, literals
, Literal, literal, nicht, positive, variable
, Variable 
)

where

import Prelude hiding ( foldr, filter )
import qualified Prelude
  
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Foldable as F
import Data.Monoid
import Data.List ( nub )
import Data.Function.Memoize


-- * variables and literals

type Variable = Int

data Literal =
     Literal { variable :: ! Variable
             , positive :: ! Bool
             }
     deriving ( Eq, Ord )

$(deriveMemoizable ''Literal)

instance Show Literal where
    show l = ( if positive l then "" else "-" )
             ++ show ( variable l )

literal :: Bool -> Variable -> Literal
literal pos v  = Literal { positive = pos, variable = v }

nicht :: Literal -> Literal 
nicht x = x { positive = not $ positive x }

-- * clauses

newtype Clause = Clause { literals :: [Literal] }
   deriving ( Eq, Ord )

instance Show ( Clause ) where
  show c = unwords ( map show (literals c) ++ [ "0" ] )

clause ::  [ Literal ] -> Clause 
clause ls = Clause ls 

-- * formulas

newtype CNF  = CNF { clauses :: [ Clause ] }

size (CNF s) = length s
                   
instance Show CNF  where
    show cnf = unlines $ map show $ clauses cnf

cnf :: [ Clause ] -> CNF 
cnf cs = CNF cs

