{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Base64 as BS64
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BSL
import Data.Text hiding (head)
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Types as HT
import Network.Wreq
import Test.Hspec
import Test.HUnit

import ServerRun
import TestConfig
import TestHelpers

main :: IO ()
main = do
    void $ withServer appPort "test/data/TestingConfig" $ do
        res <- hspec suite
        return res

suite :: Spec
suite = do
    describe "no auth" $ do
        it "fetches a page without auth" $
            req "/pub" "get" Nothing Nothing >>=
            expectHttpCode HT.ok200

    describe "domain 1 auth" $ do
        it "prompts for auth when none is provided" $
            req "/domain1" "get" Nothing Nothing >>=
            expectHttpCode HT.unauthorized401
        it "fetches a page when auth is provided" $
            req "/domain1" "get" Nothing (Just $ packBasic "foo" "bar") >>=
            expectHttpCode HT.ok200

    describe "domain 2 auth" $ do
        it "prompts for auth when none is provided" $
            req "/domain2" "get" Nothing Nothing >>=
            expectHttpCode HT.unauthorized401
        it "flunks out when bad auth is provided" $
            req "/domain2" "get" Nothing (Just $ packBasic "wrong" "credentials") >>=
            expectHttpCode HT.forbidden403
        it "fetches a page when good auth is provided" $
            req "/domain1" "get" Nothing (Just $ packBasic "foo" "bar") >>=
            expectHttpCode HT.ok200

packBasic :: String -> String -> String
packBasic a b = "Basic " ++ C.unpack (BS64.encode (C.pack (a ++ ":" ++ b)))

req :: String -> String -> Maybe String -> Maybe String -> IO (Either HC.HttpException (Response BSL.ByteString))
req u method postData authStr = try run
  where
    run = case method of
        "get"  -> getWith opts u'
        "post" -> postWith opts u' (C.pack $ maybe "" id $ postData)
        _ -> error "Not a valid request verb"
    u' = url u
    opts = case authStr of
        Nothing -> opts'
        Just x  -> opts' & header "Authorization" .~ [C.pack x]
    opts' = defaults
        & header "Accept" .~ [C.pack "*/*"]
