{-# language MultiParamTypeClasses, FlexibleContexts #-}

module Satchmo.Integer.Difference where

import Satchmo.Code
import Satchmo.Numeric 

data Number a = Difference { top :: a, bot :: a }

instance Decode a Integer 
         => Decode ( Number a ) Integer where
    decode n = do
        t <- decode $ top n
        b <- decode $ bot n
        return $ t - b
        
instance Constant a => Constant ( Number a ) where
    constant n = 
        if n >= 0 then do
            t <- constant n
            b <- constant 0
            return $ Difference { top = t, bot = b }
        else do    
            t <- constant 0
            b <- constant $ negate n
            return $ Difference { top = t, bot = b }

instance Create a => Create ( Number a ) where
    create bits = do
        t <- create bits
        b <- create bits
        return $ Difference { top = t, bot = b }

instance Numeric a => Numeric ( Number a ) where        
    equal a b = do
        t <- plus ( top a ) ( bot b )
        b <- plus ( bot a ) ( top b )
        equal t b
    greater_equal a b = do
        t <- plus ( top a ) ( bot b )
        b <- plus ( bot a ) ( top b )
        greater_equal t b      
    plus a b = do 
        t <- plus ( top a ) ( top b )
        b <- plus ( bot a ) ( bot b )
        return $ Difference { top = t, bot = b }
    minus a b = do 
        t <- plus ( top a ) ( bot b )
        b <- plus ( bot a ) ( top b )
        return $ Difference { top = t, bot = b }
    times a b = do 
        tt <- times ( top a ) ( top b )
        bb <- times ( bot a ) ( bot b )
        t  <- plus tt bb
        tb <- times ( top a ) ( bot b )
        bt <- times ( bot a ) ( top b )
        b  <- plus tb bt
        return $ Difference { top = t, bot = b }
