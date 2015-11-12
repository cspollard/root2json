module Data.Atlas.Jet where

import Data.Atlas.PtEtaPhiE

data Jet = Jet {
    jPtEtaPhiE :: PtEtaPhiE
    } deriving Show

newtype Jets = Jets [Jet] deriving Show
