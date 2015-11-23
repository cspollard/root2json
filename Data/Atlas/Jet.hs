module Data.Atlas.Jet where

import Data.Atlas.PtEtaPhiE
import Data.Vector

data Jet = Jet {
    jPtEtaPhiE :: PtEtaPhiE
    } deriving Show

newtype Jets = Jets (Vector Jet) deriving Show
