module Data.Atlas.Jet where

data Jet = Jet {
    jPt :: Double,
    jEta :: Double
    } deriving Show

newtype Jets = Jets [Jet]
