{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.HTTPAuth.Backend.Utilities.Configurator (
    cfgToAllowEverythingIfHeader,
    cfgToUserPass
) where

import qualified Data.Configurator.Types as CT
import Data.Text (Text)
import Snap.Utilities.Configuration

import Snap.Snaplet.HTTPAuth.Backend.AllowEverythingIfHeader
import Snap.Snaplet.HTTPAuth.Backend.UserPass

-------------------------------------------------------------------------------
-- | Converts Configurator pairs to an AllowEverythingIfHeader.
cfgToAllowEverythingIfHeader
    :: [(Text, CT.Value)] -- ^ Pairs of configuration values extracted from the application's configuration file
    -> AllowEverythingIfHeader -- ^ An AllowEverythingIfHeader backend for a particular HTTPAuth domain.
cfgToAllowEverythingIfHeader _ = AllowEverythingIfHeader

-- | Converts Configurator pairs to a UserPass.
cfgToUserPass
    :: [(Text, CT.Value)] -- ^ Pairs of configuration values extracted from the application's configuration file
    -> UserPass -- ^ A UserPass backend for a particular HTTPAuth domain.
cfgToUserPass cfg =
    let
        u = cfgLookupWithDefault "Username" "" stringValue cfg
        p = cfgLookupWithDefault "Password" "" stringValue cfg
        in
            UserPass u p

