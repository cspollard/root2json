module Data.Atlas.Electron where

import Data.Atlas.PtEtaPhiE

data Electron = Electron {
    eFourMom :: PtEtaPhiE
    } deriving Show
    {- 
    eTightLH :: Bool,
    eMediumLH :: Bool,
    eLooseLH :: Bool,
    eTightIso :: Bool,
    eLooseTrackOnlyIso :: Bool
    -}

newtype Electrons = Electrons [Electron] deriving Show
