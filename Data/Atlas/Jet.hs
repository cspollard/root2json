module Data.Atlas.Jet where

import Data.Atlas.PtEtaPhiE
import Data.Vector

data Jet = Jet {
    jPtEtaPhiE :: PtEtaPhiE,
    jMV2c20 :: Double,
    jJVT :: Double
    } deriving Show

type Jets = Vector Jet

data LargeJet = LargeJet {
    ljPtEtaPhiE :: PtEtaPhiE,
    ljM :: Double,
    ljSD12 :: Double
    } deriving Show

type LargeJets = Vector LargeJet

data TrackJet = TrackJet {
    tjPtEtaPhiE :: PtEtaPhiE,
    tjMV2c20 :: Double
    } deriving Show

type TrackJets = Vector TrackJet
