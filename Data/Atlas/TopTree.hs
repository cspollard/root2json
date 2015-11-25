{-# LANGUAGE OverloadedStrings #-}

module Data.Atlas.TopTree where

import Control.Applicative

import Data.Text (Text, unpack)
import Data.Aeson (Value(..), withObject, eitherDecode)
import Data.Aeson ((.:), FromJSON(..))
import Data.Aeson.Types (Parser)
import Data.Vector (Vector, (!), generateM)
import qualified Data.Vector as V

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.Attoparsec.Lazy as AL

import Data.Attoparsec.ByteString.Char8 (skipSpace, char, string, manyTill, anyChar)

import Data.Monoid ((<>))

import Data.Atlas.Event
import Data.Atlas.Electron
import Data.Atlas.Muon
import Data.Atlas.Jet
import Data.Atlas.PtEtaPhiE


bracketScan :: Char -> Char -> AL.Parser BSL.ByteString
bracketScan p q = fmap BSL.fromStrict $ BS.snoc <$> scanner <*> char q
    where
        scanner = AC.scan 0 $ \n c -> if c == p
                            then Just (n+1)
                            else if c == q
                                then if n == 1
                                    then Nothing
                                    else Just (n-1)
                                else Just n


-- return event and whether it is the last one
event :: AL.Parser (Event, Bool)
event = do
            skipSpace
            evtTxt <- bracketScan '{' '}'
            case eitherDecode evtTxt of
                Left err -> fail err
                Right evt -> ((,) evt . (/= ',')) <$> (skipSpace *> anyChar )

parseEvents :: BSL.ByteString -> [Event]
parseEvents bs = case AL.parse event bs of
                    AL.Fail _ _ err -> error err
                    AL.Done bs' (evt, False) -> evt : parseEvents bs'
                    AL.Done _ (evt, True) -> [evt]



parseTree :: BSL.ByteString -> [Event]
parseTree bs = case AL.parse headerParse bs of
                AL.Fail _ _ err -> error err
                AL.Done bs' _ -> parseEvents bs'
        where
            headerParse = manyTill anyChar (string "\"events\"") <* skipSpace <* char ':' <* skipSpace <* char '['



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
