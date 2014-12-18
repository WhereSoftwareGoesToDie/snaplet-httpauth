{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.HTTPAuth.Types.AuthUser (
    AuthUser (..)
) where

import Data.ByteString (ByteString)
import Data.Map

-------------------------------------------------------------------------------
data AuthUser = AuthUser {
    authUserIdentity :: ByteString,
    authUserDetails  :: Map ByteString ByteString
} deriving (Show, Eq)
