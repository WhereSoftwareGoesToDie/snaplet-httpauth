--
-- Copyright © 2013-2014 Anchor Systems, Pty Ltd and Others
--
-- The code in this file, and the program it is a part of, is
-- made available to you by its authors as open source software:
-- you can redistribute it and/or modify it under the terms of
-- the 3-clause BSD licence.
--

{-# LANGUAGE OverloadedStrings #-}

module App.Run (
    withServer
) where

import Control.Concurrent
import Control.Exception
import Control.Lens
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as BSL
import Data.Configurator
import Data.Map.Lazy
import Data.Text (pack)
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Types as HT
import Snap.Http.Server (simpleHttpServe)
import Snap.Http.Server.Config
import Snap.Snaplet
import System.IO

import App.App
import Test.SafeCWD

withServer :: Int -> String -> IO a -> IO a
withServer servePort cfgPath runner = bracket start stop run
  where
    start = do
        (tid, mvar) <- inDir False "." $ startServer servePort cfgPath
        putStrLn $ "Running server on process " ++ show tid
        return (tid, mvar)
    stop (tid, _) = killThread tid
    run (_,_) = runner

startServer :: Int -> String -> IO (ThreadId, MVar ())
startServer servePort cfgPath = do
    mvar <- newEmptyMVar
    t    <- forkIO $ serve mvar (setPort servePort defaultConfig) app
    threadDelay $ 2*10^(6::Int)
    return (t, mvar)
  where
    serve mvar config initializer =
        flip finally (putMVar mvar ()) $
        handle handleErr $ do
            hPutStrLn stderr "initializing snaplet"

            (_, handler, doCleanup) <- runSnaplet (Just cfgPath) initializer

            flip finally doCleanup $ do
                (conf, site) <- combineConfig config handler
                hPutStrLn stderr "bringing up server"
                simpleHttpServe conf site
                hPutStrLn stderr "server killed"

    handleErr :: SomeException -> IO ()
    handleErr e = hPutStrLn stderr $ "startServer exception: " ++ show e
