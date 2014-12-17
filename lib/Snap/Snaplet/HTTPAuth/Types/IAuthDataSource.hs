{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeFamilies              #-}

module Snap.Snaplet.HTTPAuth.Types.IAuthDataSource (
    IAuthDataSource (..),
    AuthDataWrapper (..),
    configToADT
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
type CfgPair = (Text, CT.Value)

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

-- | Builds an AuthDataWrapper out of configuration information.
-- Most often used as a partial method when setting up your site.
configToADT
    :: (IAuthDataSource r)
    => ([CfgPair] -> r) -- ^ A function that converts a list of Configurator pairs to an object of class IAuthDataSource.
    -> [CfgPair] -- ^ A list of Configurator pairs.
    -> AuthDataWrapper -- ^ A container for an arbitrary object of class IAuthDataSource.
configToADT groupTranslator cfg = AuthDataWrapper (getUser b, validateUser b)
    where
        b = groupTranslator cfg
