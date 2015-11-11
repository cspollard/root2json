{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson ((.:))
import qualified Data.Aeson as A
import Data.Aeson.Types (parseEither, Parser)
import qualified Data.ByteString.Lazy as BS
import System.Environment (getArgs)
import qualified Data.HashMap.Lazy as HM (fromList, HashMap)
import Data.Vector (Vector)
import Data.Map (Map, fromList)

import Control.Applicative


branchDict = Map String (A.Value -> Branch)
branchDict = fromList [
                    ("Bool_t", Bool)
                    ("bool", Bool)
                    ("vecto<bool>", Bool)
                    ]

data Branch = Bool Bool
            | Int Int
            | Double Double
            | VInt (Vector Int)
            | VDouble (Vector Double)
            | VVBool (Vector (Vector Bool))
            | VVInt (Vector (Vector Int))
            | VVDouble (Vector (Vector Double))


bBool :: Parser Branch
bBool = Bool $ 


type TTree = [HM.HashMap String A.Value]

parserTree :: A.Object -> Parser TTree
parserTree obj = do
                    branchNames <- fmap (map head) $ (obj .: "branches" :: Parser [[String]])
                    events <- obj .: "events" :: Parser [[A.Value]]
                    let t = map (zip branchNames) events :: [[(String, A.Value)]]
                    return . map HM.fromList $ t

main :: IO ()
main = do
    bs <- BS.readFile =<< head <$> getArgs
    let tree = parseEither parserTree `fmap` A.decode bs

    print tree
