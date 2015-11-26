{-# LANGUAGE DeriveGeneric #-}

module Data.Atlas.Electron where

import Data.Atlas.PtEtaPhiE
 
import Data.Binary
import GHC.Generics (Generic)

data Electron = Electron {
    ePtEtaPhiE :: PtEtaPhiE,
    eClEta :: Double,
    eCharge :: Double,
    eD0Sig :: Double,
    ePtVarCone20 :: Double
    } deriving (Show, Generic)

instance Binary Electron

type Electrons = [Electron]
