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
main = do
    void $ withServer appPort "test/data/TestingConfig" $ do
        res <- hspec suite
        return res

suite :: Spec
suite = do
    describe "public pages" $ do
        it "fetches a page without auth" $ do
            void $ runSession webdriverCfg $ do
                openPage $ url "/pub"
                wdIsUrl "/pub"

                b <- wdGetElems "body" 1

                tx <- getText $ head b
                liftIO $ tx `shouldBe` pack "Hello world"

                closeSession
        it "fetches a tpl with auth" $ do
            void $ runSession webdriverCfg $ do
                openPage $ url "/tpl/pub"
                wdIsUrl "/tpl/pub"

                b <- wdGetElems "h1" 1
                tx <- getText $ head b
                liftIO $ tx `shouldBe` pack "Anonymous"

                closeSession

    describe "auth everything" $ do
        it "fetches a page with auth" $ do
            void $ runSession webdriverCfg $ do
                openPage $ url "/everything"
                wdIsUrl "/everything"

                b <- wdGetElems "body" 1

                tx <- getText $ head b
                liftIO $ tx `shouldBe` pack "Hello everything"

                closeSession
        it "fetches a tpl without auth" $ do
            void $ runSession webdriverCfg $ do
                openPage $ url "/tpl/everything"
                wdIsUrl "/tpl/everything"

                b <- wdGetElems "h1" 1
                tx <- getText $ head b
                liftIO $ tx `shouldBe` pack "Anonymous"

                closeSession
        it "fetches a tpl with auth" $ do
            void $ runSession webdriverCfg $ do
                openPage $ urlWithCreds "/tpl/everything" "hello" "world"
                wdIsUrl "/tpl/everything"

                b <- wdGetElems "h1" 1
                tx <- getText $ head b
                liftIO $ tx `shouldBe` pack "hello"

                closeSession
        it "fetches a tpl with auth 2" $ do
            void $ runSession webdriverCfg $ do
                openPage $ urlWithCreds "/tpl/everything" "foo" "bar"
                wdIsUrl "/tpl/everything"

                b <- wdGetElems "h1" 1
                tx <- getText $ head b
                liftIO $ tx `shouldBe` pack "foo"

                closeSession

    describe "auth ifheader" $ do
        it "fetches a page with auth" $ do
            void $ runSession webdriverCfg $ do
                openPage $ urlWithCreds "/ifheader" "foo" "bar"
                wdIsUrl "/ifheader"

                b <- wdGetElems "body" 1

                tx <- getText $ head b
                liftIO $ tx `shouldBe` pack "Hello ifheader"

                closeSession
        it "fetches a tpl with auth" $ do
            void $ runSession webdriverCfg $ do
                openPage $ urlWithCreds "/tpl/ifheader" "hello" "world"
                wdIsUrl "/tpl/ifheader"

                b <- wdGetElems "h1" 1
                tx <- getText $ head b
                liftIO $ tx `shouldBe` pack "hello"

                closeSession
        it "fetches a tpl with auth 2" $ do
            void $ runSession webdriverCfg $ do
                openPage $ urlWithCreds "/tpl/ifheader" "foo" "bar"
                wdIsUrl "/tpl/ifheader"

                b <- wdGetElems "h1" 1
                tx <- getText $ head b
                liftIO $ tx `shouldBe` pack "foo"

                closeSession

    describe "auth userpass" $ do
        it "fetches a page with auth" $ do
            void $ runSession webdriverCfg $ do
                openPage $ urlWithCreds "/userpass" "foo" "bar"
                wdIsUrl "/userpass"

                b <- wdGetElems "body" 1

                tx <- getText $ head b
                liftIO $ tx `shouldBe` pack "Hello userpass"

                closeSession
        it "fetches a tpl with auth" $ do
            void $ runSession webdriverCfg $ do
                openPage $ urlWithCreds "/tpl/userpass" "foo" "bar"
                wdIsUrl "/tpl/userpass"

                b <- wdGetElems "h1" 1
                tx <- getText $ head b
                liftIO $ tx `shouldBe` pack "foo"

                closeSession
