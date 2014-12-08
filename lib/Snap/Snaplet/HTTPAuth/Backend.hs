{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.HTTPAuth.Backend (
    AllowEverything (..),
    AnchorAuthAPI (..),
    cfgToAllowEverything,
    cfgToAnchorAuthAPI
) where

import Snap.Snaplet.HTTPAuth.Backend.AllowEverything
