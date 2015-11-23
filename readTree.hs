import qualified Data.Aeson as A
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as BS
import System.Environment (getArgs)

import Data.Atlas.TopTree
import Data.Atlas.Event
import Data.Atlas.Jet
import Data.Atlas.PtEtaPhiE

import Control.Applicative
import Control.Monad (forM_)
import Data.Vector ((!))
 
import Data.Either (rights)

main :: IO ()
main = do
    bs <- BS.readFile =<< head <$> getArgs
    let tree = parseEither A.parseJSON `fmap` A.decode bs :: Maybe (Either String TopTree)

    case tree of
        Just (Left err) -> print err
        Just (Right (TopTree t)) -> writeEvents . rights . map (parseEither A.parseJSON) $ t
        Nothing -> print "FAIL"


writeEvents :: [Event] -> IO ()
writeEvents evts = do
                    forM_ evts $ \evt -> do
                            putStr . show . lvPt . jPtEtaPhiE . (! 0) . eJets $ evt
                            putChar '\n'
