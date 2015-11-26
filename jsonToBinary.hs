module Main where

import qualified Data.ByteString.Lazy as BSL

import Data.Atlas.Stream
import Data.Atlas.TopTree

import Data.Binary

 
main :: IO ()
main = BSL.putStr . encode . Stream . parseTree =<< BSL.getContents
