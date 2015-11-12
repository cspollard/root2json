module Data.Atlas.PtEtaPhiE where

data PtEtaPhiE = PtEtaPhiE Double Double Double Double
    deriving Show

newtype PtEtaPhiEs = PtEtaPhiEs [PtEtaPhiE]
