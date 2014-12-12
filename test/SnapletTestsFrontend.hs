{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Base64 as BS64
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromMaybe)
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
main = void $ withServer appPort "test/data/TestingConfig" $ hspec suite

suite :: Spec
suite = do
    describe "public pages" $ do
        it "fetches a page without auth" $
            req "/pub" "get" Nothing Nothing >>=
            expectHttpCodeContent HT.ok200 "Hello world"
        it "fetches a tpl with auth" $
            req "/tpl/pub" "get" Nothing Nothing >>=
            expectHttpCodeWrappedContent HT.ok200 "h1" "Anonymous"

    describe "auth everything" $ do
        it "fetches a page with auth" $
            req "/everything" "get" Nothing Nothing >>=
            expectHttpCodeContent HT.ok200 "Hello everything"
        it "fetches a tpl without auth" $
            req "/tpl/everything" "get" Nothing Nothing >>=
            expectHttpCodeWrappedContent HT.ok200 "h1" "Anonymous"
        it "fetches a tpl with auth" $
            req "/tpl/everything" "get" Nothing (Just $ packBasic "hello" "world") >>=
            expectHttpCodeWrappedContent HT.ok200 "h1" "hello"
        it "fetches a tpl with auth 2" $
            req "/tpl/everything" "get" Nothing (Just $ packBasic "foo" "bar") >>=
            expectHttpCodeWrappedContent HT.ok200 "h1" "foo"

    describe "auth ifheader" $ do
        it "fetches a page with auth" $
            req "/ifheader" "get" Nothing (Just $ packBasic "hello" "world") >>=
            expectHttpCodeContent HT.ok200 "Hello ifheader"
        it "fetches a tpl with auth" $
            req "/tpl/ifheader" "get" Nothing (Just $ packBasic "hello" "world") >>=
            expectHttpCodeWrappedContent HT.ok200 "h1" "hello"
        it "fetches a tpl with auth 2" $
            req "/tpl/ifheader" "get" Nothing (Just $ packBasic "foo" "bar") >>=
            expectHttpCodeWrappedContent HT.ok200 "h1" "foo"

    describe "auth userpass" $ do
        it "fetches a page with auth" $
            req "/userpass" "get" Nothing (Just $ packBasic "foo" "bar") >>=
            expectHttpCodeContent HT.ok200 "Hello userpass"
        it "fetches a tpl with auth" $
            req "/tpl/userpass" "get" Nothing (Just $ packBasic "foo" "bar") >>=
            expectHttpCodeWrappedContent HT.ok200 "h1" "foo"

packBasic :: String -> String -> String
packBasic a b = "Basic " ++ C.unpack (BS64.encode (C.pack (a ++ ":" ++ b)))

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
