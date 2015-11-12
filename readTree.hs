
import qualified Data.Aeson as A
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as BS
import System.Environment (getArgs)

import Data.Atlas.TopTree
import Data.Atlas.Event

import Control.Applicative

main :: IO ()
main = do
    bs <- BS.readFile =<< head <$> getArgs
    let tree = parseEither A.parseJSON `fmap` A.decode bs :: Maybe (Either String TopTree)

    case tree of
        Just (Left err) -> print err
        Just (Right (TopTree t)) -> print . map (parseEither A.parseJSON :: A.Value -> Either String Event) $ t
        Nothing -> print "FAIL"
