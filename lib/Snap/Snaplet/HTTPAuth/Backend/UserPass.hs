{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Snap.Snaplet.HTTPAuth.Backend.UserPass (
    UserPass (..),
    IAuthDataSource,
    cfgToUserPass
) where

import Prelude hiding (lookup)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.HashMap (HashMap (..), fromList, lookup)
import qualified Data.Configurator.Types as CT
import Data.Text (Text)
import Snap.Snaplet.HTTPAuth.Types.AuthHeader
import Snap.Snaplet.HTTPAuth.Types.AuthUser
import Snap.Snaplet.HTTPAuth.Types.IAuthDataSource
import Snap.Utilities.Configuration

-------------------------------------------------------------------------------
data UserPass = UserPass {
    userpassUsername :: String,
    userpassPassword :: String
}

instance IAuthDataSource UserPass where
    getUser _ Nothing  = return Nothing
    getUser up (Just (AuthHeaderWrapper (_,gf,_))) = return $
        if (gf "Username") == (Just . userpassUsername $ up)
            then (Just $
                AuthUser (C.pack . userpassUsername $ up)
                         (fromList withPasswd) )
            else Nothing
      where
        withPasswd = case gf "Password" of
            Just p  -> [("Password", C.pack p)]
            Nothing -> []
    validateUser up _ (AuthUser username f) =
        (username == (C.pack . userpassUsername $ up)) &&
        ((lookup "Password" f) == (Just . C.pack . userpassPassword $ up))

-------------------------------------------------------------------------------
cfgToUserPass :: [(Text, CT.Value)] -> UserPass
cfgToUserPass cfg =
    let
        u = cfgLookupWithDefault "Username" "" stringValue cfg
        p = cfgLookupWithDefault "Password" "" stringValue cfg
        in
            UserPass u p
