{-# LANGUAGE OverloadedStrings #-}

module Data.Atlas.TopTree where

import Control.Applicative

import Data.Text (Text, unpack)
import Data.Aeson (Value(..), object, withObject)
import Data.Aeson ((.:), FromJSON(..))
import Data.Aeson.Types (Parser)
-- import qualified Data.HashMap.Strict as HM (fromList, HashMap)
import Data.Vector (Vector, (!), generateM)
import qualified Data.Vector as V

import Data.Monoid ((<>))

import Data.Atlas.Event
import Data.Atlas.Electron
import Data.Atlas.Muon
import Data.Atlas.Jet
import Data.Atlas.PtEtaPhiE

newtype TopTree = TopTree [Value]

instance FromJSON TopTree where
    parseJSON = withObject "tree parsing expects an object" $
                    \obj -> do
                        branchNames <- fmap (map head) (obj .: "branches" :: Parser [[Text]])
                        events <- obj .: "events" :: Parser [[Value]]
                        let t = map (zip branchNames) events :: [[(Text, Value)]]
                        return . TopTree . map object $ t


parseBranch :: FromJSON a => Text -> Value -> Parser a
parseBranch name = withObject
                        ("parseBranch: the item with key " <> unpack name <> " is not an object.")
                        (.: name)


parseIdx :: FromJSON a => Text -> Int -> Value -> Parser a
parseIdx name idx val = (! idx) `fmap` parseBranch name val


parsePtEtaPhiE :: Text -> Int -> Value -> Parser PtEtaPhiE
parsePtEtaPhiE prefix idx val = PtEtaPhiE <$>
                                    parseIdx (prefix <> "pt") idx val <*>
                                    parseIdx (prefix <> "eta") idx val <*>
                                    parseIdx (prefix <> "phi") idx val <*>
                                    parseIdx (prefix <> "e") idx val


parseElectron :: Int -> Value -> Parser Electron
parseElectron idx val = Electron <$>
                            parsePtEtaPhiE "el_" idx val <*>
                            parseIdx "el_cl_eta" idx val <*>
                            parseIdx "el_charge" idx val <*>
                            parseIdx "el_d0sig" idx val <*>
                            parseIdx "el_ptvarcone20" idx val


parseMuon :: Int -> Value -> Parser Muon
parseMuon idx val = Muon <$>
                        parsePtEtaPhiE "mu_" idx val <*>
                        parseIdx "mu_charge" idx val <*>
                        parseIdx "mu_d0sig" idx val <*>
                        parseIdx "mu_ptvarcone30" idx val


parseJet :: Int -> Value -> Parser Jet
parseJet idx val = Jet <$>
                    parsePtEtaPhiE "jet_" idx val <*>
                    parseIdx "jet_mv2c20" idx val <*>
                    parseIdx "jet_jvt" idx val


parseLargeJet :: Int -> Value -> Parser LargeJet
parseLargeJet idx val = LargeJet <$>
                            parsePtEtaPhiE "ljet_" idx val <*>
                            parseIdx "ljet_m" idx val <*>
                            parseIdx "ljet_sd12" idx val


parseMET :: Value -> Parser PtEtaPhiE
parseMET val = let et = parseBranch "met_met" val in
                PtEtaPhiE <$>
                    et <*>
                    return 0.0 <*>
                    parseBranch "met_phi" val <*>
                    et


parseTreeVector :: Text -> (Int -> Value -> Parser a) -> Value -> Parser (Vector a)
parseTreeVector prefix f = withObject "parseVector expects an object." $
                    \obj -> do
                        n <- V.length <$> (obj .: (prefix <> "pt") :: Parser (Vector Double))
                        generateM n (flip f (Object obj))


{-
instance FromJSON Electrons where
    parseJSON v = Electrons <$> parseTreeVector "el_" parseElectron v

instance FromJSON Muons where
    parseJSON v = Muons <$> parseTreeVector "mu_" parseMuon v

instance FromJSON Jets where
    parseJSON v = Jets <$> parseTreeVector "jet_" parseJet v

instance FromJSON LargeJets where
    parseJSON v = LargeJets <$>
                    parseTreeVector "ljet_" parseLargeJet v
-}


instance FromJSON Event where
    parseJSON v = Event <$>
                    parseBranch "runNumber" v <*>
                    parseBranch "eventNumber" v <*>
                    parseBranch "mcChannelNumber" v <*>
                    parseBranch "weight_mc" v <*>
                    parseBranch "mu" v <*>
                    {-
                    parseJSON v <*>
                    parseJSON v <*>
                    parseJSON v <*>
                    parseJSON v <*>
                    -}
                    parseTreeVector "el_" parseElectron v <*>
                    parseTreeVector "mu_" parseMuon v <*>
                    parseTreeVector "jet_" parseJet v <*>
                    parseTreeVector "ljet_" parseLargeJet v <*>
                    parseMET v
