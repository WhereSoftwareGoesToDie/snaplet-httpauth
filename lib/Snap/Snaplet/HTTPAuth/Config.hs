{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.HTTPAuth.Config (
    getAuthManagerCfg,
    configToADT
) where

import Prelude hiding (lookup)
import Data.ByteString (ByteString)
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import Data.List (groupBy, intercalate, find, sortBy, lookup)
import Data.HashMap.Strict (toList)
import Data.Text (Text, isPrefixOf, splitOn, pack, unpack)
import GHC.TypeLits

import Snap.Snaplet.HTTPAuth.Types
import Snap.Snaplet.HTTPAuth.Backend

import Snap.Utilities.Configuration

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
    return . (AuthConfig ahp) . map (evalGroup awp) $ groups
  where
    isAD (k, _) = "AuthDomains." `isPrefixOf` k

------------------------------------------------------------------------------
-- | Evaluate a single config group.
evalGroup :: [AuthWrapPairs] -> [CfgPair] -> AuthDomain
evalGroup awp g = withValidGroup "AuthenticationType" processGroup g
  where
    processGroup name gType gCfg =
        case lookup gType awp of
            Just fn -> AuthDomain name (fn gCfg)
            _       -> error ("Not a valid AuthenticationType in " ++ name)
