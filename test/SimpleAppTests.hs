{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BSL
import Data.Monoid
import Data.Text hiding (head)
import qualified Network.HTTP.Types as HT
import Network.Wreq
import Snap.Core
import Snap.Http.Server
import System.Exit
import Test.Hspec
import Test.HUnit

import Simple.App
import Test.Config
import Test.Helpers

main :: IO ()
main = void $ withServer appPort $ hspec suite

suite :: Spec
suite = do
    describe "public pages" $ do
        it "fetches a page without auth" $
            req "/foo" "get" Nothing Nothing >>=
            expectHttpCodeContent HT.ok200 "bar"

    describe "auth userpass" $ do
        it "fetches error without auth" $
            req "/echo/test" "get" Nothing Nothing >>=
            expectHttpCode HT.unauthorized401
        it "fetches error with bad auth" $
            req "/echo/test" "get" Nothing (Just $ packBasic "no" "way") >>=
            expectHttpCode HT.forbidden403
        it "fetches a page with good auth" $
            req "/echo/test" "get" Nothing (Just $ packBasic "foo" "bar") >>=
            expectHttpCodeContent HT.ok200 "test"

withServer :: Int -> IO a -> IO a
withServer servePort runner = bracket start stop run
  where
    start  = do
        pid <- forkIO $ httpServe (setPort servePort mempty) site
        return pid
    stop  = killThread
    run _ = runner
