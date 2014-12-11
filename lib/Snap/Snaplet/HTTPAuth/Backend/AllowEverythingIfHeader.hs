{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.HTTPAuth.Backend.AllowEverythingIfHeader (
    AllowEverythingIfHeader (..),
    IAuthDataSource,
    cfgToAllowEverythingIfHeader
) where

import qualified Data.ByteString.Char8 as C
import Data.HashMap (fromList)
import Data.Maybe (fromMaybe)
import qualified Data.Configurator.Types as CT
import Data.Text (Text)

import Snap.Snaplet.HTTPAuth.Types.AuthHeader
import Snap.Snaplet.HTTPAuth.Types.AuthUser
import Snap.Snaplet.HTTPAuth.Types.IAuthDataSource

-------------------------------------------------------------------------------
data AllowEverythingIfHeader = AllowEverythingIfHeader

instance IAuthDataSource AllowEverythingIfHeader where
    getUser _ Nothing                             = return Nothing
    getUser _ (Just (AuthHeaderWrapper (_,gf,_))) = return . Just $ AuthUser (C.pack . fromMaybe "Unknown" . gf $ "Username") (fromList [])
    validateUser _ _ _                            = True

-------------------------------------------------------------------------------
cfgToAllowEverythingIfHeader :: [(Text, CT.Value)] -> AllowEverythingIfHeader
cfgToAllowEverythingIfHeader _ = AllowEverythingIfHeader
