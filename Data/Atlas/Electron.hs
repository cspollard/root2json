module Data.Atlas.Electron where

import Data.Atlas.PtEtaPhiE
import Data.Vector (Vector)

data Electron = Electron {
    eFourMom :: PtEtaPhiE
    } deriving Show

    {-
    eTightLH :: Bool,
    eMediumLH :: Bool,
    eLooseLH :: Bool,
    eTightIso :: Bool,
    eLooseTrackOnlyIso :: Bool
    } deriving Show
    -}

newtype Electrons = Electrons (Vector Electron)
    deriving Show
