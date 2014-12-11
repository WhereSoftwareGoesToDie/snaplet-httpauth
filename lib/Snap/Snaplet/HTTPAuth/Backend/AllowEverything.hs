{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.HTTPAuth.Backend.AllowEverything (
    AllowEverything (..),
    IAuthDataSource,
    cfgToAllowEverything
) where

import qualified Data.ByteString.Char8 as C
import Data.HashMap (fromList)
import Data.Maybe (fromMaybe)
import qualified Data.Configurator.Types as CT
import Data.Text (Text)

import Snap.Snaplet.HTTPAuth.Types.AuthHeader
import Snap.Snaplet.HTTPAuth.Types.AuthUser
import Snap.Snaplet.HTTPAuth.Types.IAuthDataSource

-------------------------------------------------------------------------------
data AllowEverything = AllowEverything

instance IAuthDataSource AllowEverything where
    getUser _ Nothing                             = return . Just $ AuthUser "Anonymous" (fromList [])
    getUser _ (Just (AuthHeaderWrapper (_,gf,_))) = return . Just $ AuthUser (C.pack . fromMaybe "Unknown" . gf $ "Username") (fromList [])
    validateUser _ _ _                            = True

-------------------------------------------------------------------------------
cfgToAllowEverything :: [(Text, CT.Value)] -> AllowEverything
cfgToAllowEverything _ = AllowEverything
