module Main where

import Snap.Core
import Snap.Http.Server

import Simple.App

main :: IO ()
main = quickHttpServe site
