module Data.Atlas.Muon where

import Data.Atlas.PtEtaPhiE
import Data.Vector

data Muon = Muon {
    mFourMom :: PtEtaPhiE
    } deriving Show
    {-
    mTightLH :: Bool,
    mMediumLH :: Bool,
    mLooseLH :: Bool,
    mTightIso :: Bool,
    mLooseTrackOnlyIso :: Bool
    -}

newtype Muons = Muons (Vector Muon) deriving Show
