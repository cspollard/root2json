module Data.Atlas.Event where

import Data.Atlas.Electron
import Data.Atlas.Jet

data Event = Event {
    eElectrons :: [Electron],
    eJets :: [Jet]
    } deriving Show
