module Satchmo.PolynomialSOS

(nonNegative, positive, strictlyMonotone)

where

import Prelude hiding (null,and)
import Control.Monad (foldM,replicateM)

import Satchmo.MonadSAT (MonadSAT)
import Satchmo.Polynomial 
    (NumPoly,Poly,times,add,polynomial,null,equals,constantTerm,derive)
import Satchmo.Boolean (Boolean,and)
import qualified Satchmo.BinaryTwosComplement.Op.Fixed as F

sqr :: MonadSAT m => NumPoly -> m NumPoly
sqr p = p `times` p
  
sumOfSquares :: MonadSAT m => Int -> Int -> Int -> m NumPoly
sumOfSquares coefficientBitWidth degree numPoly = do
  sqrs <- replicateM numPoly 
          $ polynomial coefficientBitWidth degree >>= sqr
  foldM add null sqrs

nonNegative :: MonadSAT m => Int -- ^ Bit width of coefficients
            -> Int -- ^ Maximum degree
            -> Int -- ^ Maximum number of polynomials
            -> NumPoly -> m Boolean
nonNegative coefficientBitWidth degree numPoly p = do
  sos <- sumOfSquares coefficientBitWidth degree numPoly
  equals sos p
  
positive :: MonadSAT m => Int -- ^ Bit width of coefficients
            -> Int -- ^ Maximum degree
            -> Int -- ^ Maximum number of polynomials
            -> NumPoly -> m Boolean
positive coefficientBitWidth degree numPoly p = do
  sos <- sumOfSquares coefficientBitWidth degree numPoly
  e1 <- equals sos p
  e2 <- F.positive $ constantTerm sos 
  and [e1, e2]

strictlyMonotone :: MonadSAT m => Int -- ^ Bit width of coefficients
            -> Int -- ^ Maximum degree
            -> Int -- ^ Maximum number of polynomials
            -> NumPoly -> m Boolean
strictlyMonotone coefficientBitWidth degree numPoly p = do
  p' <- derive p
  positive coefficientBitWidth degree numPoly p'