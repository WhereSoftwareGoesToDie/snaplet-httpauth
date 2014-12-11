{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.HTTPAuth.Backend.AllowEverythingIfHeader (
    AllowEverythingIfHeader (..),
    IAuthDataSource,
    cfgToAllowEverythingIfHeader
) where

import qualified Data.ByteString.Char8 as C
import qualified Data.Configurator.Types as CT
import Data.HashMap (fromList)
import Data.Maybe (fromMaybe)
import Data.Text (Text)

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
    getUser _ Nothing                             = return Nothing
    getUser _ (Just (AuthHeaderWrapper (_,gf,_))) = return . Just $ AuthUser (C.pack . fromMaybe "Unknown" . gf $ "Username") (fromList [])
    validateUser _ _ _                            = True

-------------------------------------------------------------------------------
cfgToAllowEverythingIfHeader
	:: [(Text, CT.Value)] -- ^ Pairs of configuration values extracted from the application's configuration file
	-> AllowEverythingIfHeader -- ^ An AllowEverythingIfHeader backend for a particular HTTPAuth domain.
cfgToAllowEverythingIfHeader _ = AllowEverythingIfHeader
