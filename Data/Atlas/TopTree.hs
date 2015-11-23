{-# LANGUAGE OverloadedStrings #-}

module Data.Atlas.TopTree where

import Control.Applicative
import Data.Maybe (catMaybes)

import Data.Word (Word8)

import Data.Text (Text, unpack)
import Data.Aeson (Value(..), object, withObject, decode)
import Data.Aeson ((.:), FromJSON(..))
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Vector (Vector, (!), generateM)
import qualified Data.Vector as V

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Attoparsec.Lazy (scan)
import qualified Data.Attoparsec.ByteString.Char8 as AC (scan)
import qualified Data.Attoparsec.Lazy as AL (Parser)
import Data.Attoparsec.ByteString.Char8 (skipSpace, char, string)

import Data.Monoid ((<>))

import Data.Atlas.Event
import Data.Atlas.Electron
import Data.Atlas.Muon
import Data.Atlas.Jet
import Data.Atlas.PtEtaPhiE


bracketScan :: Char -> Char -> AL.Parser BSL.ByteString
bracketScan p q = fmap BSL.fromStrict $
                            AC.scan 0 $ \n c -> if c == p
                                                then Just (n+1)
                                                else if c == q
                                                    then if n == 1
                                                        then Nothing
                                                        else Just (n-1)
                                                    else Just n


parseTree :: AL.Parser [Event]
parseTree = do
                skipSpace
                char '{'
                skipSpace
                string "\"branches\"" *> skipSpace *> char ':' *> skipSpace
                branchesDict <- bracketScan '{' '}'

                case fmap head $ decode branchesDict of
                    Nothing -> fail "failed to compile list of branches."
                    Just branchNames -> do
                                            skipSpace *> char ',' *> skipSpace *> "\"events\"" *> skipSpace *> char ':' *> skipSpace
                                            eventsText <- many (skipSpace *> bracketScan '{' '}' <* skipSpace <* char ',')
                                            let eventsValues = catMaybes $ map decode eventsText :: [[Value]]
                                            let eventsWithBranches = map (zip branchNames) eventsValues :: [[(Text, Value)]]
                                            return (catMaybes . map (parseMaybe parseJSON . object) $ eventsWithBranches :: [Event])


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


instance FromJSON Event where
    parseJSON v = Event <$>
                    parseBranch "runNumber" v <*>
                    parseBranch "eventNumber" v <*>
                    parseBranch "mcChannelNumber" v <*>
                    parseBranch "weight_mc" v <*>
                    parseBranch "mu" v <*>
                    parseTreeVector "el_" parseElectron v <*>
                    parseTreeVector "mu_" parseMuon v <*>
                    parseTreeVector "jet_" parseJet v <*>
                    parseTreeVector "ljet_" parseLargeJet v <*>
                    parseMET v
