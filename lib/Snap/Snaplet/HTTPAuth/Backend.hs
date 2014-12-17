{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.HTTPAuth.Backend (
    AllowEverythingIfHeader (..),
    UserPass (..),

    cfgToAllowEverythingIfHeader,
    cfgToUserPass
) where

import Snap.Snaplet.HTTPAuth.Backend.AllowEverythingIfHeader
import Snap.Snaplet.HTTPAuth.Backend.UserPass
