module Data.Atlas.Muon where

import Data.Atlas.PtEtaPhiE
import Data.Vector

data Muon = Muon {
    mFourMom :: PtEtaPhiE,
    mCharge :: Double,
    mD0Sig :: Double,
    mPtVarCone30 :: Double
    } deriving Show

newtype Muons = Muons (Vector Muon) deriving Show
