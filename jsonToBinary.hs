module Main where

import qualified Data.ByteString.Lazy as BSL

import Data.Atlas.Stream
import Data.Atlas.TopTree
import Data.Binary
import Codec.Compression.GZip

 
main :: IO ()
main = BSL.putStr . compressWith defaultCompressParams { compressLevel = bestCompression } . encode . Stream . parseTree =<< BSL.getContents
