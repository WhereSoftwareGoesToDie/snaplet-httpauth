{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.HTTPAuth.Config (
    getAuthManagerCfg,
    configToADT
) where

import Data.ByteString (ByteString)
import qualified Data.Configurator.Types as CT
import Data.List (lookup)
import Data.Text (Text, isPrefixOf)
import Prelude hiding (lookup)
import Snap.Utilities.Configuration

import Snap.Snaplet.HTTPAuth.Types

------------------------------------------------------------------------------
type CfgPair = (Text, CT.Value)
type AuthWrapPairs = (String, [CfgPair] -> AuthDataWrapper)

------------------------------------------------------------------------------
-- | Gets auth manager configuration.
getAuthManagerCfg
    :: [ByteString -> Maybe AuthHeaderWrapper]
    -> [AuthWrapPairs]
    -> CT.Config
    -> IO AuthConfig
getAuthManagerCfg ahp awp config = do
    groups <- extractGroups isAD config
    return . AuthConfig ahp . map (evalGroup awp) $ groups
  where
    isAD (k, _) = "AuthDomains." `isPrefixOf` k

------------------------------------------------------------------------------
-- | Evaluate a single config group.
evalGroup :: [AuthWrapPairs] -> [CfgPair] -> AuthDomain
evalGroup awp = withValidGroup "AuthenticationType" processGroup
  where
    processGroup name gType gCfg =
        case lookup gType awp of
            Just fn -> AuthDomain name (fn gCfg)
            _       -> error ("Not a valid AuthenticationType in " ++ name)
