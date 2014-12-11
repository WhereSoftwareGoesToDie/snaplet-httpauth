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
    configToADT
) where

import Control.Lens
import Data.ByteString (ByteString)

import Snap.Snaplet.HTTPAuth.Types.AuthHeader
import Snap.Snaplet.HTTPAuth.Types.AuthUser
import Snap.Snaplet.HTTPAuth.Types.IAuthDataSource

-------------------------------------------------------------------------------
data AuthDomain = AuthDomain {
    authDomainName   :: String,
    authDomainSource :: AuthDataWrapper
}

data AuthConfig = AuthConfig {
    _authHeaders :: [ByteString -> Maybe AuthHeaderWrapper],
    _authDomains :: [AuthDomain]
}

makeLenses ''AuthConfig
