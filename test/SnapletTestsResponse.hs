{-# LANGUAGE OverloadedStrings #-}

module Main where

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

import App.Run
import Test.Config
import Test.Helpers

main :: IO ()
main = void $ withServer appPort "test/data/TestingConfig" $ hspec suite

suite :: Spec
suite = do
    describe "no auth" $
        it "fetches a page without auth" $
            req "/pub" "get" Nothing Nothing >>=
            expectHttpCode HT.ok200

    describe "ifheader auth" $ do
        it "prompts for auth when none is provided" $
            req "/ifheader" "get" Nothing Nothing >>=
            expectHttpCode HT.unauthorized401
        it "flunks out when bad auth is provided" $
            req "/ifheader" "get" Nothing (Just $ packBasic "wrong" "credentials") >>=
            expectHttpCode HT.ok200
        it "fetches a page when auth is provided" $
            req "/ifheader" "get" Nothing (Just $ packBasic "foo" "bar") >>=
            expectHttpCode HT.ok200

    describe "userpass auth" $ do
        it "prompts for auth when none is provided" $
            req "/userpass" "get" Nothing Nothing >>=
            expectHttpCode HT.unauthorized401
        it "flunks out when bad auth is provided" $
            req "/userpass" "get" Nothing (Just $ packBasic "wrong" "credentials") >>=
            expectHttpCode HT.forbidden403
        it "fetches a page when good auth is provided" $
            req "/userpass" "get" Nothing (Just $ packBasic "foo" "bar") >>=
            expectHttpCode HT.ok200
