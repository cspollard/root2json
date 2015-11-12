module Data.Atlas.Electron where

data Electron = Electron {
    ePt :: Double,
    eEta :: Double
    } deriving Show
{- 
    ePhi :: Double,
    eE :: Double,
    eTightLH :: Bool,
    eMediumLH :: Bool,
    eLooseLH :: Bool,
    eTightIso :: Bool,
    eLooseTrackOnlyIso :: Bool
    } deriving Show
    -}

newtype Electrons = Electrons [Electron]
