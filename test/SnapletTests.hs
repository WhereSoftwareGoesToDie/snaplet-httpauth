module Main where

import Control.Monad
import Control.Monad.IO.Class
import Data.Text hiding (head)
import Test.Hspec
import Test.HUnit
import Test.WebDriver
import Test.WebDriver.Class (WebDriver)

import ServerRun
import TestConfig
import TestHelpers
import TestWebdriverConfig
import TestWebdriverHelpers

main :: IO ()
main = void $ withServer appPort "test/data/TestingConfig" $ hspec suite

suite :: Spec
suite = do
    describe "public pages" $ do
        it "fetches a page without auth" $
            void $ runSession webdriverCfg $ do
                openPage $ url "/pub"
                wdIsUrl "/pub"
                "body" `contentsShouldBe` "Hello world"
                closeSession
        it "fetches a tpl with auth" $
            void $ runSession webdriverCfg $ do
                openPage $ url "/tpl/pub"
                wdIsUrl "/tpl/pub"
                "h1" `contentsShouldBe` "Anonymous"
                closeSession

    describe "auth everything" $ do
        it "fetches a page with auth" $
            void $ runSession webdriverCfg $ do
                openPage $ url "/everything"
                wdIsUrl "/everything"
                "body" `contentsShouldBe` "Hello everything"
                closeSession
        it "fetches a tpl without auth" $
            void $ runSession webdriverCfg $ do
                openPage $ url "/tpl/everything"
                wdIsUrl "/tpl/everything"
                "h1" `contentsShouldBe` "Anonymous"
                closeSession
        it "fetches a tpl with auth" $
            void $ runSession webdriverCfg $ do
                openPage $ urlWithCreds "/tpl/everything" "hello" "world"
                wdIsUrl "/tpl/everything"
                "h1" `contentsShouldBe` "hello"
                closeSession
        it "fetches a tpl with auth 2" $
            void $ runSession webdriverCfg $ do
                openPage $ urlWithCreds "/tpl/everything" "foo" "bar"
                wdIsUrl "/tpl/everything"
                "h1" `contentsShouldBe` "foo"
                closeSession

    describe "auth ifheader" $ do
        it "fetches a page with auth" $
            void $ runSession webdriverCfg $ do
                openPage $ urlWithCreds "/ifheader" "foo" "bar"
                wdIsUrl "/ifheader"
                "body" `contentsShouldBe` "Hello ifheader"
                closeSession
        it "fetches a tpl with auth" $
            void $ runSession webdriverCfg $ do
                openPage $ urlWithCreds "/tpl/ifheader" "hello" "world"
                wdIsUrl "/tpl/ifheader"
                "h1" `contentsShouldBe` "hello"
                closeSession
        it "fetches a tpl with auth 2" $
            void $ runSession webdriverCfg $ do
                openPage $ urlWithCreds "/tpl/ifheader" "foo" "bar"
                wdIsUrl "/tpl/ifheader"
                "h1" `contentsShouldBe` "foo"
                closeSession

    describe "auth userpass" $ do
        it "fetches a page with auth" $
            void $ runSession webdriverCfg $ do
                openPage $ urlWithCreds "/userpass" "foo" "bar"
                wdIsUrl "/userpass"
                "body" `contentsShouldBe` "Hello userpass"
                closeSession
        it "fetches a tpl with auth" $
            void $ runSession webdriverCfg $ do
                openPage $ urlWithCreds "/tpl/userpass" "foo" "bar"
                wdIsUrl "/tpl/userpass"
                "h1" `contentsShouldBe` "foo"
                closeSession

contentsShouldBe :: String -> String -> WD ()
contentsShouldBe selector expected = do
    b <- wdGetElems selector 1
    tx <- getText $ head b
    liftIO $ tx `shouldBe` pack expected
