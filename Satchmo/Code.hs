{-# language MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances #-}

module Satchmo.Code 

( Decode (..)
, Decoder
)

where

import Satchmo.Data


import Data.Map ( Map )
import qualified Data.Map as M
import Data.Array

import Control.Monad.Reader


class Decode c a where decode :: c -> Decoder a

type Decoder a = Reader ( Map Variable Bool ) a

instance Decode () () where
    decode () = return ()

instance ( Decode c a, Decode d b ) => Decode ( c,d) (a,b) where
    decode (c,d) = do a <- decode c; b <- decode d; return ( a,b)

instance ( Decode c a ) => Decode [c] [a] where
    decode = mapM decode 

instance Decode a b => Decode ( Maybe a ) ( Maybe b ) where
    decode ( Just b ) = fmap Just $ decode b
    decode Nothing = return $ Nothing

instance (Ix i, Decode c a) => Decode ( Array i c) ( Array i a ) where
    decode x = do
        pairs <- sequence $ do
            (i,e) <- assocs x
            return $ do
                f <- decode e
                return (i,f)
        return $ array (bounds x) pairs
