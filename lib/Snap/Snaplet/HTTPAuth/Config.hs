{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.HTTPAuth.Config (
    getAuthManagerCfg,
    configToADT,

    cfgToAllowEverythingIfHeader,
    cfgToUserPass
) where

import Data.ByteString (ByteString)
import qualified Data.Configurator.Types as CT
import Data.List (lookup)
import Data.Text (Text, isPrefixOf)
import Prelude hiding (lookup)
import Snap.Utilities.Configuration

import Snap.Snaplet.HTTPAuth.Backend.Utilities.Configurator
import Snap.Snaplet.HTTPAuth.Types
import Snap.Snaplet.HTTPAuth.Types.IAuthDataSource.Utilities

------------------------------------------------------------------------------
type CfgPair = (Text, CT.Value)
type AuthWrapPairs = (String, [CfgPair] -> AuthDataWrapper)

------------------------------------------------------------------------------
-- | Gets auth manager configuration.
getAuthManagerCfg
    :: [ByteString -> Maybe AuthHeaderWrapper] -- ^ List of methods that parse Authorization headers into AuthHeaderWrapper objects
    -> [AuthWrapPairs] -- ^ List of tuples of functions that convert configuration into AuthDataWrapper objects, and configuration identifiers
    -> CT.Config -- ^ Configuration object obtained by parsing the application's configuration file
    -> IO AuthConfig
getAuthManagerCfg ahp awp config = do
    groups <- extractGroups isAD config
    return . AuthConfig ahp . map (evalGroup awp) $ groups
  where
    isAD (k, _) = "AuthDomains." `isPrefixOf` k

------------------------------------------------------------------------------
-- | Evaluate a single config group.
evalGroup
    :: [AuthWrapPairs] -- ^ List of tuples of functions that convert configuration into AuthDataWrapper objects, and configuration identifiers
    -> [CfgPair] -- ^ Pairs of configuration values extracted from the application's configuration file
    -> AuthDomain
evalGroup awp = withValidGroup "AuthenticationType" processGroup
  where
    processGroup name gType gCfg =
        case lookup gType awp of
            Just fn -> AuthDomain name (fn gCfg)
            _       -> error ("Not a valid AuthenticationType in " ++ name)
