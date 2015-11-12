module Data.Atlas.Event where

import Data.Atlas.Electron
import Data.Atlas.Jet

data Event = Event {
    eElectrons :: Electrons,
    eJets :: Jets
    } deriving Show
