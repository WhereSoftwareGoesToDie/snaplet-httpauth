{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Snap.Snaplet.HTTPAuth.Types.AuthUser (
    AuthUser (..)
) where

import Data.ByteString(ByteString)
import Data.HashMap

-------------------------------------------------------------------------------
data AuthUser = AuthUser {
    authUserIdentity :: ByteString,
    authUserDetails  :: HashMap ByteString ByteString
} deriving (Show, Eq)
