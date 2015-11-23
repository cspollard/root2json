{-# LANGUAGE OverloadedStrings #-}

module Data.Atlas.TopTree where

import Control.Applicative
import Control.Monad (liftM2)

import Data.Text (Text)
import Data.Aeson (Value(..), Object, object, withObject)
import Data.Aeson ((.:), FromJSON(..))
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HM (fromList, HashMap)
import Data.Vector (Vector(..), (!), generateM)
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

zipWithX :: Monad m => m [a -> b] -> m [a] -> m [b]
zipWithX = liftM2 $ zipWith ($)

-- takes a prefix and a value; returns a list of four momenta
parsePtEtaPhiE :: Text -> Int -> Value -> Parser PtEtaPhiE
parsePtEtaPhiE prefix idx = withObject "parsePtEtaPhiE expects an object" $
                    \obj -> PtEtaPhiE <$>
                        ((! idx) `fmap` (obj .: (prefix <> "pt"))) <*>
                        ((! idx) `fmap` (obj .: (prefix <> "eta"))) <*>
                        ((! idx) `fmap` (obj .: (prefix <> "phi"))) <*>
                        ((! idx) `fmap` (obj .: (prefix <> "e")))


parseElectron :: Int -> Value -> Parser Electron
parseElectron idx = withObject "parseElectron expects an object" $
                        \obj -> Electron <$> parsePtEtaPhiE "el_" idx (Object obj)

parseMuon :: Int -> Value -> Parser Muon
parseMuon idx = withObject "parseMuon expects an object" $
                        \obj -> Muon <$> parsePtEtaPhiE "mu_" idx (Object obj)

parseJet :: Int -> Value -> Parser Jet
parseJet idx = withObject "parseJet expects an object" $
                        \obj -> Jet <$> parsePtEtaPhiE "jet_" idx (Object obj)


parseTreeVector :: Text -> (Int -> Value -> Parser a) -> Value -> Parser (Vector a)
parseTreeVector prefix f = withObject "parseVector expects an object." $
                    \obj -> do
                        n <- V.length <$> (obj .: (prefix <> "pt") :: Parser (Vector Double))
                        generateM n (flip f (Object obj))

instance FromJSON Electrons where
    parseJSON v = Electrons <$> parseTreeVector "el_" parseElectron v

instance FromJSON Muons where
    parseJSON v = Muons <$> parseTreeVector "mu_" parseMuon v

instance FromJSON Jets where
    parseJSON v = Jets <$> parseTreeVector "jet_" parseJet v

instance FromJSON Event where
    parseJSON v = Event <$> parseJSON v <*> parseJSON v <*> parseJSON v
