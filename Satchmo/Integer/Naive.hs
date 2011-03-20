{-# language MultiParamTypeClasses #-}

module Satchmo.Integer.Naive where

import qualified Satchmo.Binary.Op.Flexible as B
import Satchmo.Code
import qualified Satchmo.Numeric as N

data Number = Number { top :: B.Number, bot :: B.Number }

instance Decode Number Integer where
    decode n = do
        t <- decode $ top n
        b <- decode $ bot n
        return $ t - b
        
instance N.Constant Number where
    constant n = 
        if n >= 0 then do
            t <- B.constant n
            b <- B.constant 0
            return $ Number { top = t, bot = b }
        else do    
            t <- B.constant 0
            b <- B.constant $ negate n
            return $ Number { top = t, bot = b }

instance N.Create Number where
    create bits = do
        t <- B.number bits
        b <- B.number bits
        return $ Number { top = t, bot = b }

instance N.Numeric Number where        
    equal a b = do
        t <- B.add ( top a ) ( bot b )
        b <- B.add ( bot a ) ( top b )
        B.equals t b
    greater_equal a b = do
        t <- B.add ( top a ) ( bot b )
        b <- B.add ( bot a ) ( top b )
        B.ge t b      
    plus a b = do 
        t <- B.add ( top a ) ( top b )
        b <- B.add ( bot a ) ( bot b )
        return $ Number { top = t, bot = b }
    minus a b = do 
        t <- B.add ( top a ) ( bot b )
        b <- B.add ( bot a ) ( top b )
        return $ Number { top = t, bot = b }
    times a b = do 
        tt <- B.times ( top a ) ( top b )
        bb <- B.times ( bot a ) ( bot b )
        t  <- B.add tt bb
        tb <- B.times ( top a ) ( bot b )
        bt <- B.times ( bot a ) ( top b )
        b  <- B.add tb bt
        return $ Number { top = t, bot = b }
