{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Snap.Snaplet.HTTPAuth.Backend.AllowEverythingIfHeader (
    AllowEverythingIfHeader (..),
    IAuthDataSource,
    cfgToAllowEverythingIfHeader
) where

import Data.ByteString (ByteString)
import Data.HashMap (HashMap (..), fromList)
import qualified Data.Configurator.Types as CT
import Data.Text (Text)

import Snap.Snaplet.HTTPAuth.Types.AuthHeader
import Snap.Snaplet.HTTPAuth.Types.AuthUser
import Snap.Snaplet.HTTPAuth.Types.IAuthDataSource

-------------------------------------------------------------------------------
data AllowEverythingIfHeader = AllowEverythingIfHeader

instance IAuthDataSource AllowEverythingIfHeader where
    getUser _ Nothing  = return Nothing
    getUser _ (Just _) = return . Just $ AuthUser "basicAuthAllowed" $ fromList []
    validateUser _ _ _ = True

-------------------------------------------------------------------------------
cfgToAllowEverythingIfHeader :: [(Text, CT.Value)] -> AllowEverythingIfHeader
cfgToAllowEverythingIfHeader _ = AllowEverythingIfHeader
