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
    describe "no auth" $ do
        it "fetches a page without auth" $ do
            void $ runSession webdriverCfg $ do
                openPage $ url "/pub"
                wdIsUrl "/pub"

                b <- wdGetElems "body" 1

                tx <- getText $ head b
                liftIO $ tx `shouldBe` pack "Hello world"

                closeSession

    describe "auth domain 1" $ do
        it "fetches a page without auth" $ do
            void $ runSession webdriverCfg $ do
                openPage $ url "/pub"
                wdIsUrl "/pub"

                b <- wdGetElems "body" 1

                tx <- getText $ head b
                liftIO $ tx `shouldBe` pack "Hello world"

                closeSession