module Main where

import qualified Data.ByteString.Lazy as BSL

import Data.Atlas.Stream
import Data.Atlas.TopTree

main :: IO ()
main = BSL.putStr . encodeList . parseTree =<< BSL.getContents
