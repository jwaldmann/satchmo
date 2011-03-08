{-# language MultiParamTypeClasses #-}
{-# language FlexibleContexts      #-}
{-# language UndecidableInstances  #-}
{-# language FlexibleInstances #-}

module Satchmo.Polynomial 

( Poly (), number, constant, fromCoefficients
, iszero, equals, ge, gt
, add, times
)

where

import Data.Map ( Map )
import qualified Data.Map as M
import Control.Applicative ((<$>))

import Satchmo.MonadSAT (MonadSAT)
import Satchmo.Boolean (Boolean,monadic)
import qualified Satchmo.Boolean as B
import Satchmo.Code

import qualified Satchmo.Binary.Op.Fixed as F

import Control.Monad ( forM )

-- | polynomial in one variable,
-- coefficients starting from degree zero
data Poly a = Poly [a] deriving ( Eq, Ord, Show )

type NumPoly = Poly F.Number

instance Decode a Integer => Decode (Poly a) (Poly Integer) where
    decode (Poly xs) = do
      decodedXs <- forM xs decode 
      return $ Poly decodedXs

fromCoefficients :: MonadSAT m 
                => Int    -- ^ bits
                -> [Integer] -- ^ coefficients
                -> m NumPoly
fromCoefficients width coefficients = 
    Poly <$> (forM coefficients $ F.constantWidth width)

-- | this is sort of wrong:
-- null polynomial should have degree -infty
-- but this function will return -1
degree :: Poly a -> Int
degree ( Poly xs ) = pred $ length xs

number :: MonadSAT m
       => Int -- ^ bits
       -> Int -- ^ degree
       -> m NumPoly
number bits deg = 
    Poly <$> (forM [ 0 .. deg ] $ \ i -> F.number bits)

constant :: MonadSAT m
         => Integer
         -> m NumPoly
constant 0 = return $ Poly []
constant const = do
    c <- F.constant const
    return $ Poly [c]

iszero :: MonadSAT m 
          => NumPoly
          -> m Boolean
iszero  ( Poly xs ) = do
    ns <- forM xs $ F.iszero
    B.and ns

binaryOp :: ([a] -> b) -> ([a] -> [a] -> b) -> [a] -> [a] -> b
binaryOp unary binary p1 p2 =
    case (p1,p2) of
      ([],ys) -> unary ys
      (xs,[]) -> unary xs
      (xs,ys) -> binary xs ys

fill :: MonadSAT m => NumPoly -> NumPoly -> m ([F.Number],[F.Number])
fill (Poly p1) (Poly p2) = do
  zero <- F.constant 0
  let maxL = max (length p1) (length p2)
      fill' xs = take maxL $ xs ++ repeat zero
  return (fill' p1, fill' p2)

reverseBoth :: ([a],[b]) -> ([a], [b])
reverseBoth (p1, p2) = (reverse p1, reverse p2)

equals,  ge,  gt  :: MonadSAT m => NumPoly -> NumPoly -> m Boolean
equals', ge', gt' :: MonadSAT m => [F.Number] -> [F.Number] -> m Boolean

equals p1 p2 = fill p1 p2 >>= uncurry equals'

equals' = binaryOp (\_ -> B.constant True)
          (\(x:xs) (y:ys) -> do e <- F.equals x y
                                rest <- equals' xs ys
                                B.and [e,rest]
          )

ge p1 p2 = fill p1 p2 >>= uncurry ge' . reverseBoth

ge' = binaryOp (\_ -> B.constant True)
      (\(x:xs) (y:ys) -> do gt <- F.gt x y
                            eq <- F.equals x y
                            rest <- ge' xs ys
                            monadic B.or [ return gt
                                         , B.and [ eq, rest ]]
      )

gt p1 p2 = fill p1 p2 >>= uncurry gt' . reverseBoth

gt' = binaryOp (\_ -> B.constant False)
      (\(x:xs) (y:ys) -> do gt <- F.gt x y
                            eq <- F.equals x y
                            rest <- gt' xs ys
                            monadic B.or [ return gt
                                         , B.and [ eq, rest ]]
      )

add,  times  :: MonadSAT m => NumPoly -> NumPoly -> m NumPoly
add', times' :: MonadSAT m => [F.Number] -> [F.Number] -> m [F.Number]

add (Poly p1) (Poly p2) = Poly <$> add' p1 p2

add' = binaryOp return 
       (\(x:xs) (y:ys) -> do z  <- F.add x y
                             zs <- add' xs ys
                             return $ z : zs
       )

times (Poly p1) (Poly p2) = Poly <$> times' p1 p2

times' = binaryOp (\_ -> return [])
         (\(x:xs) ys -> do zs   <- times' xs ys
                           f:fs <- forM ys $ F.times x
                           rest <- add' zs fs
                           return $ f : rest
         )