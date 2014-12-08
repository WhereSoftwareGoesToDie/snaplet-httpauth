{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import Control.Exception (SomeException, try)
import qualified Data.Text as T
import Snap.Core
import Snap.Http.Server
import Snap.Loader.Static
import Snap.Snaplet
import Snap.Snaplet.Config
import System.IO

import App
import TestConfig

main :: IO ()
main = do
    (conf, site, cleanup) <- $(loadSnapTH [| getConf |]
                                          'getActions
                                          [])

    _ <- try $ httpServe conf site :: IO (Either SomeException ())
    cleanup

getConf :: IO (Config Snap AppConfig)
getConf = commandLineAppConfig (setPort appPort defaultConfig)

getActions :: Config Snap AppConfig -> IO (Snap (), IO ())
getActions conf = do
    (msgs, site, cleanup) <- runSnaplet (Just "test/data/TestingConfig") app
    hPutStrLn stderr $ T.unpack msgs
    return (site, cleanup)
