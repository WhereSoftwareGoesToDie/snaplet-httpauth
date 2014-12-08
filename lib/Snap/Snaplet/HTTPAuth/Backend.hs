{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.HTTPAuth.Backend (
    AllowEverything (..),
    AllowEverythingIfHeader (..),
    UserPass (..),

    cfgToAllowEverything,
    cfgToAllowEverythingIfHeader,
    cfgToUserPass
) where

import Snap.Snaplet.HTTPAuth.Backend.AllowEverything
import Snap.Snaplet.HTTPAuth.Backend.AllowEverythingIfHeader
import Snap.Snaplet.HTTPAuth.Backend.UserPass
