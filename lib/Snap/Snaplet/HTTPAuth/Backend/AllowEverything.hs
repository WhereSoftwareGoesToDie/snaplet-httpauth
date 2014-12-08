{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Snap.Snaplet.HTTPAuth.Backend.AllowEverything (
    AllowEverything (..),
    IAuthDataSource,
    cfgToAllowEverything
) where

import Data.ByteString (ByteString)
import Data.HashMap (HashMap (..), fromList)
import qualified Data.Configurator.Types as CT
import Data.Text (Text)

import Snap.Snaplet.HTTPAuth.Types.AuthHeader
import Snap.Snaplet.HTTPAuth.Types.AuthUser
import Snap.Snaplet.HTTPAuth.Types.IAuthDataSource

-------------------------------------------------------------------------------
data AllowEverything = AllowEverything

instance IAuthDataSource AllowEverything where
    getUser _ Nothing = return . Just $ AuthUser "blankAuthHeaderAllowed" $ fromList []
    getUser _ (Just (AuthHeaderWrapper ("BasicAuth",_,_)))   = return . Just $ AuthUser "basicAuthAllowed" $ fromList []
    getUser _ (Just (AuthHeaderWrapper ("AATokenAuth",_,_))) = return . Just $ AuthUser "aaTokenAuthAllowed" $ fromList []
    getUser _ _ = return . Just $ AuthUser "unimplementedAuthAllowed" $ fromList []
    validateUser _ _ _ = True

-------------------------------------------------------------------------------
cfgToAllowEverything :: [(Text, CT.Value)] -> AllowEverything
cfgToAllowEverything _ = AllowEverything
