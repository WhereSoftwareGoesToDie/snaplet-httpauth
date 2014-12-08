{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}

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
    getUser      :: r -> Maybe AuthHeaderWrapper -> IO (Maybe AuthUser)
    validateUser :: r -> [String] -> AuthUser -> Bool

------------------------------------------------------------------------
type CfgPair = (Text, CT.Value)

data AuthDataWrapper = AuthDataWrapper ((Maybe AuthHeaderWrapper -> IO (Maybe AuthUser)), ([String] -> AuthUser -> Bool))

configToADT :: (IAuthDataSource r) => ([CfgPair] -> r) -> [CfgPair] -> AuthDataWrapper
configToADT groupTranslator cfg = AuthDataWrapper (getUser b, validateUser b)
    where
        b = groupTranslator cfg
