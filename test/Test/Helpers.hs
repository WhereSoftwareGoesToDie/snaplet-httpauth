{-# LANGUAGE OverloadedStrings #-}

module Test.Helpers where

import Control.Exception
import Control.Lens
import qualified Data.ByteString.Base64 as BS64
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.List.Split
import Data.Maybe (fromMaybe)
import Data.Text (pack)
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Types as HT
import Network.Wreq
import Test.Hspec
import Test.Hspec.Expectations
import Test.HUnit

import Test.Config

-- | Just pass something
pass :: Expectation
pass = return ()

-- | Builds a URL
url :: String -> String
url x = concat ["http://localhost:", show appPort, x]

-- | Builds a URL
urlWithCreds :: String -> String -> String -> String
urlWithCreds x u p = concat ["http://", u, ":", p, "@localhost:", show appPort, x]

-- | Unpacks a URL
unurl :: String -> String
unurl = last . splitOn ("localhost:" ++ show appPort)

-- | Packs a Basic header
packBasic :: String -> String -> String
packBasic a b = "Basic " ++ C.unpack (BS64.encode (C.pack (a ++ ":" ++ b)))

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

-- | Catchall for confirming response HTTP code matches
expectHttpCodeContent :: HT.Status -> String -> Either HC.HttpException (Response BSL.ByteString) -> Expectation
expectHttpCodeContent status content r = case r of
    Right r' -> do
        (r' ^. responseStatus) `shouldBe` status
        BSL8.unpack (r' ^. responseBody)   `shouldBe` content
    Left ex  -> error . show $ ex

-- | Catchall for confirming response HTTP code matches
expectHttpCodeWrappedContent :: HT.Status -> String -> String -> Either HC.HttpException (Response BSL.ByteString) -> Expectation
expectHttpCodeWrappedContent status wrap content r = case r of
    Right r' -> do
        (r' ^. responseStatus) `shouldBe` status
        BSL8.unpack (r' ^. responseBody) `shouldContain` c'
    Left ex  -> error . show $ ex
  where
      c' = concat ["<", wrap, ">", content, "</", wrap, ">"]

-- | Run a HTTP request
req :: String -> String -> Maybe String -> Maybe String -> IO (Either HC.HttpException (Response BSL.ByteString))
req u method postData authStr = try run
  where
    run = case method of
        "get"  -> getWith opts u'
        "post" -> postWith opts u' (C.pack $ fromMaybe "" postData)
        _ -> error "Not a valid request verb"
    u' = url u
    opts = case authStr of
        Nothing -> opts'
        Just x  -> opts' & header "Authorization" .~ [C.pack x]
    opts' = defaults
        & header "Accept" .~ [C.pack "*/*"]
