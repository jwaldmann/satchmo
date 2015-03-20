{-# language TypeFamilies #-}
{-# language GeneralizedNewtypeDeriving #-}

module Satchmo.Data 

( CNF, cnf, singleton, clauses, foldr, filter, size, assign
, Clause (CTrue), clause, literals, without
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

newtype CNF  = CNF ( S.Set Clause )

instance Monoid CNF where
  mempty = CNF S.empty
  mappend (CNF a) (CNF b) = CNF $ S.delete CTrue $ S.union a b

foldr f x (CNF s) = F.foldr f x s
filter p (CNF s) = CNF $ S.filter p s

size (CNF s) = S.size s
                   
clauses (CNF s) = F.toList s

instance Show CNF  where
    show cnf = unlines $ map show $ clauses cnf

cnf :: [ Clause ] -> CNF 
cnf cs = CNF $ S.fromList $ Prelude.filter ( /= CTrue) cs

singleton c = CNF $ S.singleton c

assign :: Variable -> Bool -> CNF -> CNF
assign v p (CNF s) = ( F.foldMap $ \ c -> singleton $ case c of
       CTrue -> CTrue
       Clause m -> case M.lookup v m of
         Nothing -> Clause m
         Just q ->
           if p == q then CTrue
           else Clause $ M.delete v m ) s

data Clause = Clause  ! ( M.Map Variable Bool )  | CTrue
   deriving ( Eq, Ord )

literals :: Clause ->  [ Literal ]
literals c = case c of
  Clause m -> map ( \ (v,p) -> literal p v ) $ M.toList m

instance Monoid Clause where
  mempty = Clause M.empty
  mappend c1 c2 = case c1 of
    CTrue -> CTrue
    Clause m1 -> case c2 of
      CTrue -> CTrue
      Clause m2 ->
        let common = M.intersection m1 m2
        in  if M.isSubmapOf common m1 && M.isSubmapOf common m2
            then Clause $ M.union m1 m2
            else CTrue

instance Show ( Clause ) where
  show c = case c of
    CTrue -> "# True"
    Clause m -> unwords ( map show (literals c) ++ [ "0" ] )

clause ::  [ Literal ] -> Clause 
clause ls = Prelude.foldr
            ( \ l c -> case c of
                 CTrue -> CTrue           
                 Clause m -> case M.lookup (variable l) m of
                   Nothing -> Clause $ M.insert (variable l) (positive l) m
                   Just p -> if p == positive l then Clause m else CTrue
            ) mempty ls

without c w = case c of
  -- CTrue -> CTrue -- ?
  Clause m -> Clause $ M.filterWithKey ( \ v p -> w /= v ) m
