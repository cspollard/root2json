module Data.Atlas.Jet where

import Data.Atlas.PtEtaPhiE
import Data.Vector

data Jet = Jet {
    jPtEtaPhiE :: PtEtaPhiE,
    jMV2c20 :: Double,
    jJVT :: Double
    } deriving Show

newtype Jets = Jets (Vector Jet) deriving Show

data LargeJet = LargeJet {
    ljPtEtaPhiE :: PtEtaPhiE,
    ljM :: Double,
    ljSD12 :: Double
    } deriving Show

newtype LargeJets = LargeJets (Vector LargeJet) deriving Show

data TrackJet = TrackJet {
    tjPtEtaPhiE :: PtEtaPhiE,
    tjMV2c20 :: Double
    } deriving Show

newtype TrackJets = TrackJets (Vector TrackJet) deriving Show
