module Data.Atlas.Event where

import Data.Atlas.Electron
import Data.Atlas.Muon
import Data.Atlas.Jet

data Event = Event {
    eElectrons :: Electrons,
    eMuons :: Muons,
    eJets :: Jets
    } deriving Show
