-- from
-- https://stackoverflow.com/questions/6205294/binary-serialization-for-lists-of-undefined-length-in-haskell

module Data.Atlas.Stream where

import Data.Binary

newtype StreamList a = Stream { unstream :: [a] }

instance Binary a => Binary (Stream a) where

    put (Stream [])     = putWord8 0
    put (Stream (x:xs)) = putWord8 1 >> put x >> put (Stream xs)

    get = do
        t <- getWord8
        case t of
            0 -> return (Stream [])
            1 -> do x         <- get
                    Stream xs <- get
                    return (Stream (x:xs))
