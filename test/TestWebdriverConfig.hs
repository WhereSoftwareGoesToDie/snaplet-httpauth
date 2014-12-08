module TestWebdriverConfig where

import Test.WebDriver

import TestConfig

webdriverCfg :: WDConfig
webdriverCfg = defaultConfig { wdCapabilities = defaultCaps { browser = chrome } }
