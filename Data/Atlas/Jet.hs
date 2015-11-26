{-# LANGUAGE DeriveGeneric #-}

module Data.Atlas.Jet where

import Data.Atlas.PtEtaPhiE

import Data.Binary
import GHC.Generics (Generic)

data Jet = Jet {
    jPtEtaPhiE :: PtEtaPhiE,
    jMV2c20 :: Double,
    jJVT :: Double
    } deriving (Show, Generic)

instance Binary Jet

instance LorentzVector Jet where
    lvPt = lvPt . jPtEtaPhiE
    lvEta = lvEta . jPtEtaPhiE
    lvPhi = lvPhi . jPtEtaPhiE
    lvE = lvE . jPtEtaPhiE

type Jets = [Jet]

data LargeJet = LargeJet {
    ljPtEtaPhiE :: PtEtaPhiE,
    ljM :: Double,
    ljSD12 :: Double
    } deriving (Show, Generic)

instance Binary LargeJet

instance LorentzVector LargeJet where
    lvPt = lvPt . ljPtEtaPhiE
    lvEta = lvEta . ljPtEtaPhiE
    lvPhi = lvPhi . ljPtEtaPhiE
    lvE = lvE . ljPtEtaPhiE

type LargeJets = [LargeJet]

data TrackJet = TrackJet {
    tjPtEtaPhiE :: PtEtaPhiE,
    tjMV2c20 :: Double
    } deriving (Show, Generic)

instance Binary TrackJet

instance LorentzVector TrackJet where
    lvPt = lvPt . tjPtEtaPhiE
    lvEta = lvEta . tjPtEtaPhiE
    lvPhi = lvPhi . tjPtEtaPhiE
    lvE = lvE . tjPtEtaPhiE

type TrackJets = [TrackJet]
