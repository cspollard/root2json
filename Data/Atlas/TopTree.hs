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

type TopTree = [Value]

-- go from an Aeson Object to TopTree
parserTree :: Object -> Parser TopTree
parserTree obj = do
                    branchNames <- fmap (map head) (obj .: "branches" :: Parser [[Text]])
                    events <- obj .: "events" :: Parser [[Value]]
                    let t = map (zip branchNames) events :: [[(Text, Value)]]
                    return . map object $ t

zipWithX :: Monad m => m [a -> b] -> m [a] -> m [b]
zipWithX = liftM2 $ zipWith ($)

electrons :: Value -> Parser [Electron]
electrons = withObject "electron parsing expects an object" $
                    \obj -> return (repeat Electron) `zipWithX` (obj .: "el_pt") `zipWithX` (obj .: "el_eta")


jets :: Value -> Parser [Jet]
jets = withObject "jet parsing expects an object" $
                    \obj -> return (repeat Jet) `zipWithX` (obj .: "jet_pt") `zipWithX` (obj .: "jet_eta")

instance FromJSON Event where
    parseJSON v = Event <$> electrons v <*> jets v
