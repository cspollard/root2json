module Data.Atlas.Event where

import Data.Atlas.Electron
import Data.Atlas.Muon
import Data.Atlas.Jet

data Event = Event {
    eRunNumber :: Int,
    eEventNumber :: Int,
    eMCChannelNumber :: Int,
    eMCWeight :: Double,
    eMu :: Double,
    eElectrons :: Electrons,
    eMuons :: Muons,
    eJets :: Jets,
    eLargeJets :: LargeJets
    } deriving Show
