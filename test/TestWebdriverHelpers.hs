module TestWebdriverHelpers where

import Control.Monad.IO.Class
import Data.Text (pack)
import Test.Hspec
import Test.HUnit
import Test.WebDriver

import TestHelpers
import TestWebdriverConfig

-- | Helper function to ensure we are at the URL that we expect to be.
wdIsUrl :: String -> WD ()
wdIsUrl expected = do
    u <- getCurrentURL
    let u' = unurl u
    liftIO . assertEqual (errorMsg expected u') expected $ u'
  where
    errorMsg x y = "Expected to be at URL " ++ x ++ " but was actually at " ++ y

-- | Helper function to fetch a whole bunch of items and ensure that we have as many as we expect.
wdGetElems :: String -> Int -> WD [Element]
wdGetElems selector expected = do
    e <- findElems $ ByCSS $ pack selector
    liftIO $ assertEqual errorMsg expected $ length e
    return e
  where
    errorMsg = "should find " ++ show expected ++ " '" ++ show selector ++ "' elem" ++ (if expected == 1 then "" else "s")

-- | Helper function to ensure we have as many items as we expect.
wdHasElems :: String -> Int -> WD ()
wdHasElems selector expected = do
    _ <- wdGetElems selector expected
    return ()
