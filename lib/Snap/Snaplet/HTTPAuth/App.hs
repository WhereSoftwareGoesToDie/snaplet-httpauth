{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}

module Snap.Snaplet.HTTPAuth.App (
    withAuth,
    withAuth',
    withAuthDomain,
    currentUser,
    currentUserInDomain
) where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.List hiding (lookup)
import Prelude hiding (lookup)
import Snap.Core
import Snap.Snaplet

import Snap.Snaplet.HTTPAuth.Types

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
        Just d  -> currentUser' h d
  where
    currentUser' fs (AuthDomain _ (AuthDataWrapper (gu, _))) = do
        maybe_h <- getHeader "Authorization" <$> getRequest
        case maybe_h >>= parseAuthorizationHeader fs of
            Nothing -> return Nothing
            Just h  -> liftIO $ gu h

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
    withTop auth (authDomain dn) >>= withAuthDomain' dn [] auth ifSuccessful

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
    >>= withAuthDomain' dn addRoles auth ifSuccessful

-- | Internal method: Uses the application lens to extract the Auth header
-- parsers, and hands over to withAuthDomain.
withAuthDomain'
    :: String -- ^ HTTPAuth domain name matching one of the domains defined in
              --   the AuthDomains config
    -> [String] -- ^ List of additional roles to add to this domain to
                --   authenticate with
    -> SnapletLens b AuthConfig -- ^ Lens to this application's AuthConfig
    -> Handler b b () -- ^ Handler to run if authentication was successful
    -> Maybe AuthDomain -- ^ A potential AuthDomain object determined by the
                        --   supplied domain name
    -> Handler b b ()
withAuthDomain' _ _ _ _ Nothing = throwDenied
withAuthDomain' dn add_roles auth success_k (Just ad) = do
    fs <- withTop auth $ view authHeaders
    withAuthDomain dn add_roles fs success_k (Just ad)

-- | Perform authentication passthrough with a known AuthDomain and list of
-- additional roles.
withAuthDomain
    :: (MonadSnap m)
    => String -- ^ HTTPAuth domain name matching one of the domains defined in
              --   the AuthDomains config
    -> [String] -- ^ List of additional roles to add to this domain to
                --   authenticate with
    -> [ByteString -> Maybe AuthHeaderWrapper] -- ^ List of Auth header parsers.
    -> m () -- ^ Handler to run if authentication was successful
    -> Maybe AuthDomain -- ^ A potential AuthDomain object determined by the
                        --   supplied domain name
    -> m ()
withAuthDomain _ _ _ _ Nothing = throwDenied
withAuthDomain dn add_roles fs success_k (Just ad) = do
    maybe_hdr <- getHeader "Authorization" <$> getRequest
    case maybe_hdr >>= parseAuthorizationHeader fs of
        Nothing -> (throwChallenge dn)
        Just h  -> do
            success <- liftIO $ testAuthHeader ad add_roles h
            if success then success_k else throwDenied

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

--------------------------------------------------------------------------------
-- | Internal method: Throw a 401 error response.
throwChallenge
    :: (MonadSnap m)
    => String -- ^ HTTPAuth domain name matching one of the domains defined in the AuthDomains config
    -> m ()
throwChallenge domainName = do
    modifyResponse $ setResponseStatus 401 "Unauthorized" . setHeader "WWW-Authenticate" (C.pack realm)
    writeBS "Tell me about yourself"
  where
    realm = "Basic realm=" ++ domainName

-- | Internal method: Throw a 403 error response.
throwDenied
    :: (MonadSnap m)
    => m ()
throwDenied = do
    modifyResponse $ setResponseStatus 403 "Access Denied"
    writeBS "Access Denied"

--------------------------------------------------------------------------------
-- | Internal method: Test authentication header against AuthDomain, using the
-- current AuthDomain's implementation of of validateUser.
testAuthHeader
    :: AuthDomain -- ^ An AuthDomain object determined by the supplied domain name
    -> [String] -- ^ List of additional roles to add to this domain to authenticate with
    -> AuthHeaderWrapper -- ^ An AuthHeaderWrapper obtained by successfully parsing the Authorization header
    -> IO Bool
testAuthHeader (AuthDomain _ (AuthDataWrapper (gu, vu))) addRoles h = do
    x <- gu h
    return $ maybe False (vu addRoles) x
