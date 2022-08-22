{-# language MultiParamTypeClasses #-}
{-# language FlexibleContexts      #-}
{-# language UndecidableInstances  #-}
{-# language FlexibleInstances #-}

module Satchmo.Polynomial 

( Poly (Poly), NumPoly, polynomial, constant, fromCoefficients
, isNull, null, constantTerm, coefficients
, equals, ge, gt
, add, times, subtract, compose, apply, derive
)

where

import Prelude hiding (subtract,null)
import Data.Map ( Map )
import qualified Data.Map as M
import Control.Applicative ((<$>))
import Control.Monad (foldM)

import Satchmo.MonadSAT (MonadSAT)
import Satchmo.Boolean (Boolean,monadic)
import qualified Satchmo.Boolean as B
import Satchmo.Code

import qualified Satchmo.BinaryTwosComplement.Op.Fixed as F
--import qualified Satchmo.Binary.Op.Fixed as F

import Control.Monad ( forM )

-- | polynomial in one variable,
-- coefficients starting from degree zero
data Poly a = Poly [a] deriving ( Eq, Ord, Show )

type NumPoly = Poly F.Number

instance Decode m a Integer => Decode m (Poly a) (Poly Integer) where
    decode (Poly xs) = do
      decodedXs <- forM xs decode 
      return $ Poly decodedXs

fromCoefficients :: MonadSAT m => Int -- ^ Bits
                 -> [Integer]         -- ^ Coefficients
                 -> m NumPoly
fromCoefficients width coefficients = 
    Poly <$> (forM coefficients $ F.constantWidth width)

polynomial :: MonadSAT m => Int -- ^ Bits
           -> Int -- ^ Degree
           -> m NumPoly
polynomial bits deg = 
    Poly <$> (forM [ 0 .. deg ] $ \ i -> F.number bits)

constant :: MonadSAT m
         => Integer
         -> m NumPoly
constant 0 = return $ Poly []
constant const = do
    c <- F.constant const
    return $ Poly [c]

-- | this is sort of wrong:
-- null polynomial should have degree -infty
-- but this function will return -1
degree :: Poly a -> Int
degree ( Poly xs ) = pred $ length xs

isNull :: Poly a -> Bool
isNull (Poly []) = True
isNull _         = False

null :: Poly a
null = Poly []

constantTerm :: Poly a -> a
constantTerm (Poly (c:_)) = c

coefficients :: Poly a -> [a]
coefficients (Poly cs) = cs

fill :: MonadSAT m => NumPoly -> NumPoly -> m ([F.Number],[F.Number])
fill (Poly p1) (Poly p2) = do
  zero <- F.constant 0
  let maxL = max (length p1) (length p2)
      fill' xs = take maxL $ xs ++ repeat zero
  return (fill' p1, fill' p2)

reverseBoth :: ([a],[b]) -> ([a], [b])
reverseBoth (p1, p2) = (reverse p1, reverse p2)

binaryOp :: ([a] -> b) -> ([a] -> [a] -> b) -> [a] -> [a] -> b
binaryOp unary binary p1 p2 =
    case (p1,p2) of
      ([],ys) -> unary ys
      (xs,[]) -> unary xs
      (xs,ys) -> binary xs ys

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

add,  times, subtract, compose :: MonadSAT m => NumPoly -> NumPoly -> m NumPoly
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
                           ~(f:fs) <- forM ys $ F.times x
                           rest <- add' zs fs
                           return $ f : rest
         )

subtract (Poly p1) (Poly p2) = do
  p2' <- forM p2 F.negate
  Poly <$> add' p1 p2'

-- | @compose p(x) q(x) = p(q(x))@
compose (Poly p1) (Poly p2) = 
    let p:ps = reverse p1
    in do
      Poly <$> compose' [p] ps p2

compose' zs = binaryOp (\_  -> return zs)
              (\(x:xs) ys -> do zs' <- zs `times'` ys >>= add' [x] 
                                compose' zs' xs ys
              )

-- | @apply p x@ applies number @x@ to polynomial @p@
apply :: MonadSAT m => NumPoly -> F.Number -> m F.Number
apply (Poly poly) x = 
    let p:ps = reverse poly
    in 
      foldM (\sum -> F.linear sum x) p ps

-- | @derive p@ computes the derivation of @p@
derive :: MonadSAT m => NumPoly -> m NumPoly
derive (Poly p) = 
    let p' = zip p [0..]
        dx (x,e) = F.constant e >>= F.times x
    in
      (Poly . drop 1) <$> forM p' dx
      