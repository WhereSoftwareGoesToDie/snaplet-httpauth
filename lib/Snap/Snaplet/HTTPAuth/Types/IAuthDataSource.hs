{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeFamilies              #-}

module Snap.Snaplet.HTTPAuth.Types.IAuthDataSource (
    IAuthDataSource (..),
    AuthDataWrapper (..)
) where

import qualified Data.Configurator.Types as CT
import Data.Text (Text)

import Snap.Snaplet.HTTPAuth.Types.AuthHeader
import Snap.Snaplet.HTTPAuth.Types.AuthUser

------------------------------------------------------------------------
class IAuthDataSource r where
    getUser      :: r -> AuthHeaderWrapper -> IO (Maybe AuthUser)
    validateUser :: r -> [String] -> AuthUser -> Bool

------------------------------------------------------------------------
-- | A wrapper around an object that implements the IAuthDataSource class.
-- Contains a tuple of two functions:
-- (getUser) a function that takes a Maybe AuthHeaderWrapper and returns
-- a Maybe AuthUser,
-- and (validateUser) a function that takes a list of roles specific to
-- the Handler, and an AuthUser. It returns True if the user is allowed
-- to run this handler, and False if not.
data AuthDataWrapper = AuthDataWrapper {
    authDataUnwrap :: (AuthHeaderWrapper -> IO (Maybe AuthUser), [String] -> AuthUser -> Bool)
}
