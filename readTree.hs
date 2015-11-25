import qualified Data.ByteString.Lazy as BS
import Data.Attoparsec.ByteString.Lazy (parse, eitherResult)
import System.Environment (getArgs)

import Data.Atlas.TopTree
import Data.Atlas.Event
import Data.Atlas.Jet
import Data.Atlas.PtEtaPhiE

import Control.Applicative
import Control.Monad (forM_)
import Data.Vector ((!?))
 
main :: IO ()
main = do
    bs <- BS.getContents
    writeEvents $ parseTree bs


printJust :: Show a => Maybe a -> IO ()
printJust Nothing = return ()
printJust (Just x) = putStr $ show x ++ " "

writeEvents :: [Event] -> IO ()
writeEvents evts = do
                    forM_ evts $ \evt -> do
                            printJust . fmap lvPt . fmap jPtEtaPhiE . (!? 0) . eJets $ evt
                            printJust . fmap lvEta . fmap jPtEtaPhiE . (!? 0) . eJets $ evt
                            printJust . fmap lvPhi . fmap jPtEtaPhiE . (!? 0) . eJets $ evt
                            putChar '\n'
