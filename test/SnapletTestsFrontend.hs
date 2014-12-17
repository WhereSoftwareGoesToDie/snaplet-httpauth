{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BSL
import Data.Text hiding (head)
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
            expectHttpCodeWrappedContent HT.ok200 "h1" "Nobody"

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
