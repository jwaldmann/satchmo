{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleContexts      #-}

module Satchmo.PolynomialN
    ( Coefficient, Exponents, PolynomialN (), NumPolynomialN
    , fromMonomials, add, equals)
where

import Control.Monad (forM,foldM)
import Data.List (partition,sortBy)
import qualified Satchmo.Binary.Op.Fixed as F
import Satchmo.Code (Decode (..),decode)
import Satchmo.MonadSAT (MonadSAT)
import Satchmo.Boolean (Boolean)
import qualified Satchmo.Boolean as B

type Coefficient a = a

type Exponents = [Integer]

data Monomial a  = Monomial (Coefficient a, Exponents) deriving (Show)
type NumMonomial = Monomial F.Number

data PolynomialN a  = PolynomialN [Monomial a] deriving (Show)
type NumPolynomialN = PolynomialN F.Number

instance Decode m a Integer => Decode m (Monomial a) (Monomial Integer) where
    decode (Monomial (coeff,vars)) = do
      decodedCoeff <- decode coeff
      return $ Monomial (decodedCoeff,vars)

instance Decode m a Integer => Decode m (PolynomialN a) (PolynomialN Integer) where
    decode (PolynomialN monomials) = do
        decodedMonomials <- forM monomials decode
        return $ PolynomialN decodedMonomials

fromMonomials :: MonadSAT m 
              => Int -- ^ bit width of coefficients
              -> [(Coefficient Integer,Exponents)] -- ^ monomials
              -> m NumPolynomialN
fromMonomials bits monomials = do
  monomials' <- forM monomials $ \(c,es) -> do
                                 coefficient <- F.constantWidth bits c
                                 return $ Monomial (coefficient,es)
  reduce $ PolynomialN monomials'

coefficient :: Monomial a -> Coefficient a
coefficient (Monomial (c,_)) = c

exponents :: Monomial a -> Exponents
exponents (Monomial (_,e)) = e

monomials :: PolynomialN a -> [Monomial a]
monomials (PolynomialN xs) = xs

sameExponents :: Monomial a -> Monomial a -> Bool
sameExponents m1 m2 = exponents m1 == exponents m2

add :: MonadSAT m => NumPolynomialN -> NumPolynomialN -> m NumPolynomialN
add (PolynomialN xs) (PolynomialN ys) =
    reduce $ PolynomialN $ xs ++ ys

addMonomial :: MonadSAT m => NumMonomial -> NumMonomial -> m NumMonomial
addMonomial m1 m2 =
    if sameExponents m1 m2 then 
        do c <- F.add (coefficient m1) (coefficient m2)
           return $ Monomial (c, exponents m1)
    else
        error "PolynomialN.addMonomial"

strictOrdering :: Monomial a -> Monomial a -> Ordering
strictOrdering (Monomial (_,xs)) (Monomial (_,ys)) = compare xs ys

reduce :: MonadSAT m => NumPolynomialN -> m NumPolynomialN
reduce (PolynomialN []) = return $ PolynomialN []
reduce (PolynomialN (x:xs)) =
    let (reducable,notReducable) = partition (sameExponents x) xs
        strictOrd (Monomial (_,xs)) (Monomial (_,ys)) = compare xs ys
    in do
      newMonomial <- foldM addMonomial x reducable
      PolynomialN rest <- reduce $ PolynomialN notReducable
      return $ PolynomialN $ sortBy strictOrd $ newMonomial : rest
    
equalsMonomial :: MonadSAT m => NumMonomial -> NumMonomial -> m Boolean
equalsMonomial m1 m2 = do
  equalsCoefficient <- F.equals (coefficient m1) (coefficient m2)
  equalsExponents <- B.constant $ (exponents m1) == (exponents m2)
  B.and [equalsCoefficient,equalsExponents]

equals :: MonadSAT m => NumPolynomialN -> NumPolynomialN -> m Boolean
equals (PolynomialN []) (PolynomialN []) = B.constant True
equals (PolynomialN (x:xs)) (PolynomialN (y:ys)) = do
  e <- equalsMonomial x y
  es <- equals (PolynomialN xs) (PolynomialN ys)
  B.and [e,es]
