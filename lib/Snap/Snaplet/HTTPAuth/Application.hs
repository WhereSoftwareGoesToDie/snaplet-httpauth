{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.HTTPAuth.Application (
    authInit,

    withAuth,
    withAuth',
    withAuthDomain,
    currentUser,
    currentUserInDomain,

    cfgToAllowEverythingIfHeader,
    cfgToUserPass,

    getAuthManagerCfg,
    configToADT,
    AuthHeaderWrapper (..)
) where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.List hiding (lookup)
import Data.Monoid
import Data.Text.Encoding (decodeUtf8)
import Heist
import Prelude hiding (lookup)
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import qualified Text.XmlHtml as X

import Snap.Snaplet.HTTPAuth.Authorise
import Snap.Snaplet.HTTPAuth.Backend.Utilities.Configurator
import Snap.Snaplet.HTTPAuth.Config
import Snap.Snaplet.HTTPAuth.Types

--------------------------------------------------------------------------------
-- | Initialise HTTPAuth snaplet.
authInit
    :: AuthConfig               -- ^ Configuration
    -> SnapletInit b AuthConfig -- ^ The initialised HTTPAuth Snaplet
authInit =
    makeSnaplet "auth" "Handles user authentication" Nothing . return

--------------------------------------------------------------------------------
-- | Public method: Get current user from Auth headers.
currentUser
    :: SnapletLens b AuthConfig -- ^ Lens to this application's AuthConfig
    -> Handler b b (Maybe AuthUser)
currentUser = currentUserInDomain "display"

-- | Public method: Get current user from Auth headers within an arbitrary
-- domain.
currentUserInDomain
    :: String -- ^ HTTPAuth domain name matching one of the domains defined
              --   in the AuthDomains config
    -> SnapletLens b AuthConfig -- ^ Lens to this application's AuthConfig
    -> Handler b b (Maybe AuthUser)
currentUserInDomain domain_name auth = do
    x <- withTop auth $ authDomain domain_name
    h <- withTop auth $ view authHeaders
    case x of
        Nothing -> return Nothing
        Just d  -> userFromDomain h d

--------------------------------------------------------------------------------
-- | Perform authentication passthrough.
--
-- This version only uses roles that are derived from the AuthDomain itself.
withAuth
    :: String -- ^ HTTPAuth domain name matching one of the domains defined
              -- in the AuthDomains config
    -> SnapletLens b AuthConfig -- ^ Lens to this application's AuthConfig
    -> Handler b b () -- ^ Handler to run if authentication was successful
    -> Handler b b () -- ^ If the user was successfully authenticated, the
                      --    provided handler will be run. Otherwise, internal
                      --    handlers returning a 401 Unauthorized (when no
                      --    header was found) or a 403 Access Denied (when a
                      --    header was found but not authorised) HTTP status
                      --    code will be returned.
withAuth dn auth ifSuccessful =
    withTop auth (authDomain dn) >>= withAuthDomain' [] auth ifSuccessful

-- | Perform authentication passthrough.
--
-- This version uses roles that are derived from the AuthDomain, PLUS sets of
-- additional roles defined in the addRoles list.
-- This allows us to define roles that the current user must have be present
-- to work on particular assets, on top of ones already present.
withAuth'
    :: String -- ^ HTTPAuth domain name matching one of the domains defined
              --   in the AuthDomains config
    -> SnapletLens b AuthConfig -- ^ Lens to this application's AuthConfig
    -> [String] -- ^ List of additional roles to add to this domain to
                --   authenticate with
    -> Handler b b () -- ^ Handler to run if authentication was successful
    -> Handler b b ()
withAuth' dn auth addRoles ifSuccessful =
    withTop auth (authDomain dn)
    >>= withAuthDomain' addRoles auth ifSuccessful

-- | Internal method: Uses the application lens to extract the Auth header
-- parsers, and hands over to withAuthDomain.
withAuthDomain'
    :: [String] -- ^ List of additional roles to add to this domain to
                --   authenticate with
    -> SnapletLens b AuthConfig -- ^ Lens to this application's AuthConfig
    -> Handler b b () -- ^ Handler to run if authentication was successful
    -> Maybe AuthDomain -- ^ A potential AuthDomain object determined by the
                        --   supplied domain name
    -> Handler b b ()
withAuthDomain' _ _ _ Nothing = throwDenied
withAuthDomain' add_roles auth success_k (Just ad) = do
    fs <- withTop auth $ view authHeaders
    withAuthDomain add_roles fs (Just ad) success_k

--------------------------------------------------------------------------------
-- | Internal method: Get AuthDomain by name
authDomain
    :: String -- ^ HTTPAuth domain name matching one of the domains defined in the AuthDomains config
    -> Handler b AuthConfig (Maybe AuthDomain)
authDomain domainName = do
    x <- get
    return $ find domainMatch (_authDomains x)
  where
    domainMatch d = domainName == authDomainName d
