{-# language MultiParamTypeClasses #-}
{-# language FlexibleContexts      #-}
{-# language UndecidableInstances  #-}

module Satchmo.Polynomial 

( Number ()
, number, constant
, iszero, equals, ge, gt
, add, times
)

where

import Data.Map ( Map )
import qualified Data.Map as M

import Satchmo.SAT
import Satchmo.Boolean hiding ( constant )
import qualified Satchmo.Boolean 
import Satchmo.Code

import qualified Satchmo.Binary.Op.Fixed as F

import Control.Monad ( forM )

-- | polynomial in one variable,
-- coefficients starting from degree zero
data Poly a = Poly [a] deriving ( Eq, Ord, Show )

type Number = Poly F.Number

-- Hohoho:
instance Decode a Integer => Decode ( Poly a ) Integer where
    decode ( Poly xs ) = do
        ys <- forM xs decode 
        let base = 1000 -- well
        return $ if all ( < base ) ys
                 then foldr ( \ y o -> o * base + y ) 0 ys
                 else error "Satchmo.Polynomial.decode"

-- | this is sort of wrong:
-- null polynomial should have degree -infty
-- but this function will return -1
degree :: Poly a -> Int
degree ( Poly xs ) = pred $ length xs


number :: MonadSAT m
       => Int -- ^ bits
       -> Int -- ^ degree
       -> m ( Poly F.Number )
number bits deg = do 
    xs <- forM [ 0 .. deg ] $ \ i -> F.number bits
    return $ Poly xs

constant :: MonadSAT m
         => Integer
         -> m ( Poly F.Number )
constant 0 = return $ Poly []
constant c = do
    z <- F.constant 0
    o <- F.constant c
    return $ Poly [ z, o ]

iszero  ( Poly xs ) = do
    ns <- forM xs $ F.iszero
    Satchmo.Boolean.and ns

equals ( Poly xs ) ( Poly ys ) = do
          z <- F.constant 0
          let n = max ( length xs ) ( length ys )
              fill xs = take n $ xs ++ repeat z
          let handle xs ys = case ( xs, ys ) of
                  ( [], [] ) -> Satchmo.Boolean.constant True
                  (x:xs, y:ys) -> do
                      e <- F.equals x y
                      later <- handle xs ys
                      Satchmo.Boolean.and [ e, later ]
          handle ( reverse $ fill xs ) ( reverse $ fill ys )

ge ( Poly xs ) ( Poly ys ) = do
          z <- F.constant 0
          let n = max ( length xs ) ( length ys )
              fill xs = take n $ xs ++ repeat z
          let handle xs ys = case ( xs, ys ) of
                  ( [], [] ) -> Satchmo.Boolean.constant True
                  (x:xs, y:ys) -> do
                      gt <- F.gt x y
                      e <- F.equals x y
                      later <- handle xs ys
                      monadic Satchmo.Boolean.or 
                              [ return gt
                              , Satchmo.Boolean.and [ e, later ]
                              ]
          handle ( reverse $ fill xs ) ( reverse $ fill ys )

gt  ( Poly xs ) ( Poly ys ) = do
          z <- F.constant 0
          let n = max ( length xs ) ( length ys )
              fill xs = take n $ xs ++ repeat z
          let handle xs ys = case ( xs, ys ) of
                  ( [], [] ) -> Satchmo.Boolean.constant False
                  (x:xs, y:ys) -> do
                      gt <- F.gt x y
                      e <- F.equals x y
                      later <- handle xs ys
                      monadic Satchmo.Boolean.or 
                              [ return gt
                              , Satchmo.Boolean.and [ e, later ]
                              ]
          handle ( reverse $ fill xs ) ( reverse $ fill ys )


add ( Poly xs ) ( Poly ys ) = do
          let handle xs ys = case ( xs, ys ) of
                  ( [] , ys ) ->  return ys
                  ( xs, [] ) -> return xs
                  (x:xs, y:ys) -> do
                      z <- F.add x y
                      zs <- handle xs ys
                      return $ z : zs
          zs <- handle xs ys
          return $ Poly zs

times p q = do
          let handle ( Poly xs ) ( Poly ys ) = 
                  case ( xs, ys ) of
                      ( [], ys ) -> return $ Poly []
                      ( xs, [] ) -> return $ Poly []
                      ( x : xs, ys ) -> do
                          Poly zs <- handle ( Poly xs ) ( Poly ys )
                          f : fs  <- forM ys $ F.times x
                          Poly rest <- add ( Poly zs ) ( Poly fs )
                          return $ Poly $ f : rest
          handle p q

