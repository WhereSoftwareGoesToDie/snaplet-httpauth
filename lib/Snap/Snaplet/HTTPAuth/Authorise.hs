{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}

module Snap.Snaplet.HTTPAuth.Authorise (
    withAuthDomain,
    userFromAuthDataSource,
    userFromDomain,

    throwDenied
) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.Maybe
import qualified Data.Traversable as T
import Snap.Core
import Snap.Snaplet

import Snap.Snaplet.HTTPAuth.Types

--------------------------------------------------------------------------------
-- | Get current user from current Request and any IAuthDataSource.
userFromAuthDataSource
    :: (MonadSnap m, IAuthDataSource a)
    => [ByteString -> Maybe AuthHeaderWrapper] -- ^ List of available Authorization header parsers
    -> a -- ^ an AuthDataSource
    -> m (Maybe AuthUser)
userFromAuthDataSource hdr_parsers s = user' hdr_parsers (getUser s)

-- | Get current user from current Request and an AuthDomain.
userFromDomain
    :: (MonadSnap m)
    => [ByteString -> Maybe AuthHeaderWrapper] -- ^ List of available Authorization header parsers
    -> AuthDomain -- ^ an AuthDomain
    -> m (Maybe AuthUser)
userFromDomain hdr_parsers = user' hdr_parsers . fst . authDataUnwrap . authDomainSource

-- | Internal method: Get user when we have a user getter available.
user'
    :: (MonadSnap m)
    => [ByteString -> Maybe AuthHeaderWrapper] -- ^ List of available Authorization header parsers
    -> (AuthHeaderWrapper -> IO (Maybe AuthUser)) -- ^ Method to get AuthUser object from an AuthHeaderWrapper
    -> m (Maybe AuthUser)
user' hdr_parsers user_getter = do
    auth_header <- getAuthorizationHeader hdr_parsers
    case auth_header of
        Nothing -> return Nothing
        Just h  -> liftIO $ user_getter h

--------------------------------------------------------------------------------
-- | Perform authentication passthrough with a known AuthDomain and list of
-- additional roles.
withAuthDomain
    :: (MonadSnap m)
    => [String] -- ^ List of additional roles to add to this domain to
                --   authenticate with
    -> [ByteString -> Maybe AuthHeaderWrapper] -- ^ List of Auth header parsers.
    -> Maybe AuthDomain -- ^ A potential AuthDomain object determined by the
                        --   supplied domain name
    -> m () -- ^ Snap handler to run if authentication was successful
    -> m ()
withAuthDomain _ _ Nothing _ = throwDenied
withAuthDomain add_roles fs (Just ad) success_k = do
    auth_header <- getAuthorizationHeader fs
    case auth_header of
        Nothing -> throwChallenge
        Just h  -> do
            success <- liftIO $ testAuthHeader ad add_roles h
            if success then success_k else throwDenied

--------------------------------------------------------------------------------
-- | Internal method: attempt to parse Authorization header from Request.
getAuthorizationHeader
    :: (MonadSnap m)
    => [ByteString -> Maybe AuthHeaderWrapper] -- ^ List of available Authorization header parsers
    -> m (Maybe AuthHeaderWrapper)
getAuthorizationHeader hdr_parsers =
    getRequest >>=
    return . join . fmap (parseAuthorizationHeader hdr_parsers) . getHeader "Authorization"

--------------------------------------------------------------------------------
-- | Internal method: Throw a 401 error response.
throwChallenge
    :: (MonadSnap m)
    => m ()
throwChallenge = do
    modifyResponse $ setResponseStatus 401 "Unauthorized"
    writeBS "Tell me about yourself"

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
testAuthHeader (AuthDomain _ (AuthDataWrapper (gu, vu))) addRoles h =
    gu h >>= return . maybe False (vu addRoles)
