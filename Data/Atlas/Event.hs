{-# LANGUAGE DeriveGeneric #-}

module Data.Atlas.Event where

import Data.Atlas.PtEtaPhiE
import Data.Atlas.Electron
import Data.Atlas.Muon
import Data.Atlas.Jet

import Data.Binary
import GHC.Generics (Generic)

data Event = Event {
    eRunNumber :: Int,
    eEventNumber :: Int,
    eMCChannelNumber :: Int,
    eMCWeight :: Double,
    eMu :: Double,
    eElectrons :: Electrons,
    eMuons :: Muons,
    eJets :: Jets,
    eLargeJets :: LargeJets,
    eMET :: PtEtaPhiE
    } deriving (Show, Generic)

instance Binary Event

type Events = [Event]
