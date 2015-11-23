module Data.Atlas.Electron where

import Data.Atlas.PtEtaPhiE
import Data.Vector (Vector)

data Electron = Electron {
    eFourMom :: PtEtaPhiE,
    eClEta :: Double,
    eCharge :: Double,
    eD0Sig :: Double,
    ePtVarCone20 :: Double
    } deriving Show

type Electrons = Vector Electron
