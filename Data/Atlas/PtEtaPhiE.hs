{-# LANGUAGE DeriveGeneric #-}

module Data.Atlas.PtEtaPhiE where

import Data.Binary
import GHC.Generics (Generic)

data PtEtaPhiE = PtEtaPhiE Double Double Double Double
    deriving (Show, Generic)

instance Binary PtEtaPhiE

type PtEtaPhiEs = [PtEtaPhiE]

class LorentzVector a where
    lvPt :: a -> Double
    lvEta :: a -> Double
    lvPhi :: a -> Double
    lvE :: a -> Double


instance LorentzVector PtEtaPhiE where
    lvPt (PtEtaPhiE pt _ _ _) = pt
    lvEta (PtEtaPhiE _ eta _ _) = eta
    lvPhi (PtEtaPhiE _ _ phi _) = phi
    lvE (PtEtaPhiE _ _ _ e) = e
