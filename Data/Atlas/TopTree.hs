{-# LANGUAGE OverloadedStrings #-}

module Data.Atlas.TopTree where

import Control.Applicative
import Control.Monad (liftM2)

import Data.Text (Text)
import Data.Aeson (Value, Object, object, withObject)
import Data.Aeson ((.:), FromJSON(..))
import Data.Aeson.Types (Parser)
import qualified Data.HashMap.Strict as HM (fromList, HashMap)
import Data.Vector (Vector(..), (!))

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

ptEtaPhiEs :: Text -> Value -> Parser [PtEtaPhiE]
ptEtaPhiEs prefix = withObject "ptEtaPhiE parsing expects an object" $
                    \obj -> return (repeat PtEtaPhiE) `zipWithX`
                        (obj .: (prefix <> "pt")) `zipWithX`
                        (obj .: (prefix <> "eta")) `zipWithX`
                        (obj .: (prefix <> "phi")) `zipWithX`
                        (obj .: (prefix <> "e"))

instance FromJSON Electrons where
    parseJSON v = Electrons <$> (return (repeat Electron) `zipWithX` ptEtaPhiEs "el_" v)

instance FromJSON Muons where
    parseJSON v = Muons <$> (return (repeat Muon) `zipWithX` ptEtaPhiEs "mu_" v)

instance FromJSON Jets where
    parseJSON v = Jets <$> (return (repeat Jet) `zipWithX` ptEtaPhiEs "jet_" v)

instance FromJSON Event where
    parseJSON v = Event <$> parseJSON v <*> parseJSON v <*> parseJSON v
