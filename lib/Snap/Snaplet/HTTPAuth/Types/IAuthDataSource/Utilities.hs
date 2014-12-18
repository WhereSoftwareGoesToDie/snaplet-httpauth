{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeFamilies              #-}

module Snap.Snaplet.HTTPAuth.Types.IAuthDataSource.Utilities (
    configToADT
) where

import qualified Data.Configurator.Types as CT
import Data.Text (Text)

import Snap.Snaplet.HTTPAuth.Types.IAuthDataSource

------------------------------------------------------------------------
type CfgPair = (Text, CT.Value)

-- | Builds an AuthDataWrapper out of configuration information.
-- Most often used as a partial method when setting up your site.
configToADT
    :: (IAuthDataSource r)
    => ([CfgPair] -> r) -- ^ A function that converts a list of Configurator pairs to an value of class IAuthDataSource.
    -> [CfgPair] -- ^ A list of Configurator pairs.
    -> AuthDataWrapper -- ^ A container for an arbitrary value of class IAuthDataSource.
configToADT groupTranslator = wrapDataSource . groupTranslator
