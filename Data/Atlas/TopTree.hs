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

import Data.Atlas.Event
import Data.Atlas.Electron
import Data.Atlas.Jet

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

instance FromJSON Electrons where
    parseJSON v = Electrons <$> electrons v

electrons :: Value -> Parser [Electron]
electrons = withObject "electron parsing expects an object" $
                    \obj -> return (repeat Electron) `zipWithX` (obj .: "el_pt") `zipWithX` (obj .: "el_eta")


instance FromJSON Jets where
    parseJSON v = Jets <$> jets v

jets :: Value -> Parser [Jet]
jets = withObject "jet parsing expects an object" $
                    \obj -> return (repeat Jet) `zipWithX` (obj .: "jet_pt") `zipWithX` (obj .: "jet_eta")

instance FromJSON Event where
    parseJSON v = Event <$> parseJSON v <*> parseJSON v
