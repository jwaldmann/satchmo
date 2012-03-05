{-# language MultiParamTypeClasses, FunctionalDependencies #-}
{-# language FlexibleInstances, UndecidableInstances, FlexibleContexts #-}

module Satchmo.Code 

( Decode (..)
-- , Decoder
)

where

import Satchmo.Data

import Data.Array

import Control.Monad.Reader
import qualified Data.Map as M

class Monad m => Decode m c a where 
    decode :: c -> m a

-- type Decoder a = Reader ( Map Variable Bool ) a
-- type Decoder a = Reader ( Array Variable Bool ) a

instance Monad m => Decode m () () where
    decode () = return ()

instance (  Decode m c a, Decode m d b ) => Decode m ( c,d) (a,b) where
    decode (c,d) = do a <- decode c; b <- decode d; return ( a,b)

instance (  Decode m c a ) => Decode m [c] [a] where
    decode = mapM decode 

instance Decode m a b => Decode m ( Maybe a ) ( Maybe b ) where
    decode ( Just b ) = do a <- decode b ; return $ Just a
    decode Nothing = return $ Nothing

instance (Ix i, Decode m c a) => Decode m ( Array i c) ( Array i a ) where
    decode x = do
        pairs <- sequence $ do
            (i,e) <- assocs x
            return $ do
                f <- decode e
                return (i,f)
        return $ array (bounds x) pairs

instance (Ord i, Decode m c a) => Decode m ( M.Map i c) ( M.Map i a ) where
    decode x = do
        pairs <- sequence $ do
            (i,e) <- M.assocs x
            return $ do
                f <- decode e
                return (i,f)
        return $ M.fromList pairs
