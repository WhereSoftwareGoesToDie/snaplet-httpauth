{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.HTTPAuth.Backend.AllowEverything (
    AllowEverything (..),
    IAuthDataSource,
    cfgToAllowEverything
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
-- | The AllowEverything backend for the HTTPAuth Snaplet allows all requests
-- through, even if no Authorization header was provided.
-- This is probably more useful for your Heist snaplets or for testing your
-- application than it is for production.
data AllowEverything = AllowEverything

instance IAuthDataSource AllowEverything where
    getUser _ Nothing                             = return . Just $ AuthUser "Anonymous" (fromList [])
    getUser _ (Just (AuthHeaderWrapper (_,gf,_))) = return . Just $ AuthUser (C.pack . fromMaybe "Unknown" . gf $ "Username") (fromList [])
    validateUser _ _ _                            = True

-------------------------------------------------------------------------------
cfgToAllowEverything
	:: [(Text, CT.Value)] -- ^ Pairs of configuration values extracted from the application's configuration file
	-> AllowEverything -- ^ An AllowEverything backend for a particular HTTPAuth domain.
cfgToAllowEverything _ = AllowEverything
