module TestHelpers where

import Control.Lens
import qualified Data.ByteString.Lazy as BSL
import Data.Text (pack)
import Test.Hspec
import Test.HUnit
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Types as HT
import Network.Wreq

import TestConfig

-- | Just pass something
pass :: Expectation
pass = return ()

-- | Builds a URL
url :: String -> String
url x = concat ["http://localhost:", show appPort, x]

-- | Unpacks a URL
unurl :: String -> String
unurl x = drop (length . url $ "") x

-- | Catchall for confirming response HTTP code matches
expectHttpCode :: HT.Status -> Either HC.HttpException (Response BSL.ByteString) -> Expectation
expectHttpCode status r = if HT.statusIsSuccessful status
	then expectGoodHttpCode status r
	else expectBadHttpCode status r

-- | Only for good codes
expectGoodHttpCode :: HT.Status -> Either HC.HttpException (Response BSL.ByteString) -> Expectation
expectGoodHttpCode status r = case r of
    Left ex  -> error . show $ ex
    Right r' -> (r' ^. responseStatus) `shouldBe` status

-- | Only for error codes
expectBadHttpCode :: HT.Status -> Either HC.HttpException (Response BSL.ByteString) -> Expectation
expectBadHttpCode status r = case r of
    Left (HC.StatusCodeException s _ _) -> s `shouldBe` status
    Right r'                            -> (r' ^. responseStatus) `shouldBe` status
    Left ex                             -> error . show $ ex
