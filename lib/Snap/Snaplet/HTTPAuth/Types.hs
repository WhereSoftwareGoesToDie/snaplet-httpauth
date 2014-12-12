{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module Snap.Snaplet.HTTPAuth.Types (
    AuthConfig (..),
    AuthDomain (..),
    AuthUser (..),

    authDomains,
    authHeaders,

    AuthHeaderWrapper (..),
    parseBasicAuthHeader,
    parserToAHW,
    parseAuthorizationHeader,

    IAuthDataSource (..),
    AuthDataWrapper (..),
    configToADT,

    defaultAuthHeaders,
    defaultAuthDomains
) where

import Control.Lens
import Data.ByteString (ByteString)

import Snap.Snaplet.HTTPAuth.Backend (cfgToAllowEverything, cfgToAllowEverythingIfHeader, cfgToUserPass)

import Snap.Snaplet.HTTPAuth.Types.AuthHeader
import Snap.Snaplet.HTTPAuth.Types.AuthUser
import Snap.Snaplet.HTTPAuth.Types.IAuthDataSource

-------------------------------------------------------------------------------
-- | AuthDomain objects represent a certain domain that Authorization headers
-- may be passed to and authenticated against. These domains can be as simple
-- as allowing all requests through, but they can also have a full
-- authentication backend behind them.
-- AuthDomains are defined by a domain name, which matches configuration names
-- set in your configuration file, and they have a source which is derived from
-- a particular source. See `configToADT` for more details on the method that
-- creates these source objects.
data AuthDomain = AuthDomain {
    authDomainName   :: String,
    authDomainSource :: AuthDataWrapper
}

-- | AuthConfig objects are composed of two parts: a list of methods that
-- parse Authorization headers, and a list of authentication domains that can
-- apply to particular handlers.
-- The idea is that once your authentication header is parsed, then it is
-- passed to the domain specified by the handler, and then authenticated
-- against whatever source that domain is attached to.
data AuthConfig = AuthConfig {
    _authHeaders :: [ByteString -> Maybe AuthHeaderWrapper],
    _authDomains :: [AuthDomain]
}

makeLenses ''AuthConfig

-- | Set up default configuration for all the AuthHeader parsers we have
-- implemented in HTTPAuth Snaplet.
-- Covers Basic Authorization header parsing only.
defaultAuthHeaders :: [ByteString -> Maybe AuthHeaderWrapper]
defaultAuthHeaders = [parserToAHW parseBasicAuthHeader]

-- | Set up default configuration for all the AuthDomains we have
-- implemented in HTTPAuth Snaplet.
-- Covers AllowEverything, IfHeader, and UserPass backends.
defaultAuthDomains :: [(String, [CfgPair] -> AuthDataWrapper)]
defaultAuthDomains = [ ("AllowEverything", configToADT cfgToAllowEverything)
                     , ("IfHeader",        configToADT cfgToAllowEverythingIfHeader)
                     , ("UserPass",        configToADT cfgToUserPass)]
