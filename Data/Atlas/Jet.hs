{-# LANGUAGE DeriveGeneric #-}

module Data.Atlas.Jet where

import Data.HEP.LorentzVector

import Data.Binary
import GHC.Generics (Generic)

data Jet = Jet {
    jPtEtaPhiE :: PtEtaPhiE,
    jMV2c20 :: Double,
    jJVT :: Double
    } deriving (Show, Generic)

instance Binary Jet

type Jets = [Jet]

data LargeJet = LargeJet {
    ljPtEtaPhiE :: PtEtaPhiE,
    ljM :: Double,
    ljSD12 :: Double
    } deriving (Show, Generic)

instance Binary LargeJet

type LargeJets = [LargeJet]

data TrackJet = TrackJet {
    tjPtEtaPhiE :: PtEtaPhiE,
    tjMV2c20 :: Double
    } deriving (Show, Generic)

instance Binary TrackJet

type TrackJets = [TrackJet]
