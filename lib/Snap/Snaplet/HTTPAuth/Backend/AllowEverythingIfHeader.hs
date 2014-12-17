{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.HTTPAuth.Backend.AllowEverythingIfHeader (
    AllowEverythingIfHeader (..),
    IAuthDataSource
) where

import qualified Data.ByteString.Char8 as C
import Data.Map (fromList)
import Data.Maybe (fromMaybe)

import Snap.Snaplet.HTTPAuth.Types.AuthHeader
import Snap.Snaplet.HTTPAuth.Types.AuthUser
import Snap.Snaplet.HTTPAuth.Types.IAuthDataSource

-------------------------------------------------------------------------------
-- | The AllowEverythingIfHeader backend for the HTTPAuth Snaplet allows only
-- requests that have an Authorization header through.
-- This is probably more useful for your Heist snaplets or for testing your
-- application than it is for production.
data AllowEverythingIfHeader = AllowEverythingIfHeader

instance IAuthDataSource AllowEverythingIfHeader where
    getUser _ (AuthHeaderWrapper (_,gf,_)) =
        return . Just $ AuthUser (C.pack . fromMaybe "Unknown" . gf $ "Username") (fromList [])
    validateUser _ _ _                            = True
