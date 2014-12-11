{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE DisambiguateRecordFields #-}

module Snap.Snaplet.HTTPAuth.App (
    withAuth,
    withAuth',
    currentUser,
    currentUserInDomain
) where

import Prelude hiding (lookup)
import Control.Lens
import Control.Monad.State
import qualified Data.ByteString.Char8 as C
import Data.List hiding (lookup)
import Snap.Core
import Snap.Snaplet

import Snap.Snaplet.HTTPAuth.Types

--------------------------------------------------------------------------------
-- | Public method: Get current user from Auth headers.
currentUser
    :: SnapletLens b AuthConfig
    -> Handler b b (Maybe AuthUser)
currentUser = currentUserInDomain "display"

-- | Public method: Get current user from Auth headers within an arbitrary
-- domain.
currentUserInDomain
    :: String
    -> SnapletLens b AuthConfig
    -> Handler b b (Maybe AuthUser)
currentUserInDomain domainName auth = do
    x <- withTop auth (authDomain domainName)
    h <- withTop auth $ view authHeaders
    case x of
        Nothing -> return Nothing
        Just d  -> currentUser' h d
  where
    currentUser' h (AuthDomain _ (AuthDataWrapper (gu, _))) =
        getRequest >>= liftIO . gu . parseAuthorizationHeader h . getHeader "Authorization"

--------------------------------------------------------------------------------
-- | Public method: Perform authentication passthrough.
-- This version only uses roles that are derived from the AuthDomain itself.
withAuth
    :: String
    -> SnapletLens b AuthConfig
    -> Handler b b ()
    -> Handler b b ()
withAuth dn auth ifSuccessful = withTop auth (authDomain dn) >>= withAuthDomain dn [] auth ifSuccessful

-- | Public method: Perform authentication passthrough.
-- This version uses roles that are derived from the AuthDomain, PLUS sets of
-- additional roles defined in the addRoles list.
-- This allows us to define roles that the current user must have be present
-- to work on particular assets, on top of ones already present.
withAuth'
    :: String
    -> SnapletLens b AuthConfig
    -> [String]
    -> Handler b b ()
    -> Handler b b ()
withAuth' dn auth addRoles ifSuccessful = withTop auth (authDomain dn) >>= withAuthDomain dn addRoles auth ifSuccessful

-- | Internal method: Perform authentication passthrough with a known 
-- AuthDomain and list of additional roles.
withAuthDomain
    :: String
    -> [String]
    -> SnapletLens b AuthConfig
    -> Handler b b ()
    -> Maybe AuthDomain
    -> Handler b b ()
withAuthDomain dn addRoles auth ifSuccessful ad =
    case ad of
        Nothing -> throwDenied
        Just ad'@(AuthDomain _ _) -> do
            rq <- getRequest
            h <- withTop auth $ view authHeaders
            let h' = parseAuthorizationHeader h $ getHeader "Authorization" rq
            testResult <- liftIO $ testAuthHeader ad' addRoles h'
            if testResult
                then
                    ifSuccessful
                else
                    case h' of
                        Nothing -> throwChallenge dn
                        _       -> throwDenied

--------------------------------------------------------------------------------
-- | Internal method: Get AuthDomain by name
authDomain
    :: String
    -> Handler b AuthConfig (Maybe AuthDomain)
authDomain domainName = do
    x <- get
    return $ find domainMatch (_authDomains x)
  where
    domainMatch d = domainName == authDomainName d

--------------------------------------------------------------------------------
-- | Internal method: Throw a 401 error response.
throwChallenge
    :: String
    -> Handler b b ()
throwChallenge domainName = do
    modifyResponse $ setResponseStatus 401 "Unauthorized" . setHeader "WWW-Authenticate" (C.pack realm)
    writeBS "Tell me about yourself"
  where
    realm = "Basic realm=" ++ domainName

-- | Internal method: Throw a 403 error response.
throwDenied
    :: Handler b b ()
throwDenied = do
    modifyResponse $ setResponseStatus 403 "Access Denied"
    writeBS "Access Denied"

--------------------------------------------------------------------------------
-- | Internal method: Test authentication header against AuthDomain, using the
-- current AuthDomain's implementation of of validateUser.
testAuthHeader
    :: AuthDomain
    -> [String]
    -> Maybe AuthHeaderWrapper
    -> IO Bool
testAuthHeader (AuthDomain _ (AuthDataWrapper (gu, vu))) addRoles h = do
    x <- gu h
    return $ maybe False (vu addRoles) x
