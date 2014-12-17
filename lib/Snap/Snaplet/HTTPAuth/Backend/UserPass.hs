{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.HTTPAuth.Backend.UserPass (
    UserPass (..),
    IAuthDataSource,
    cfgToUserPass
) where

import qualified Data.ByteString.Char8 as C
import qualified Data.Configurator.Types as CT
import Data.HashMap (fromList, lookup)
import Data.Text (Text)
import Prelude hiding (lookup)
import Snap.Snaplet.HTTPAuth.Types.AuthHeader
import Snap.Snaplet.HTTPAuth.Types.AuthUser
import Snap.Snaplet.HTTPAuth.Types.IAuthDataSource
import Snap.Utilities.Configuration

-------------------------------------------------------------------------------
-- | The UserPass backend for the HTTPAuth Snaplet allows only requests that
-- match a single username and password through.
-- Given that it only supports a single username and password, this is probably
-- more useful for your Heist snaplets or for testing your application than it
-- is for production, but it might serve as a base for developing other
-- authentication solutions.
data UserPass = UserPass {
    userpassUsername :: String,
    userpassPassword :: String
}

instance IAuthDataSource UserPass where
    getUser up (AuthHeaderWrapper (_,gf,_)) = return $
        if gf "Username" == (Just . userpassUsername $ up)
            then Just $
                AuthUser (C.pack . userpassUsername $ up)
                         (fromList withPasswd)
            else Nothing
      where
        withPasswd = case gf "Password" of
            Just p  -> [("Password", C.pack p)]
            Nothing -> []
    validateUser up _ (AuthUser username f) =
        (username == (C.pack . userpassUsername $ up)) &&
        (lookup "Password" f == (Just . C.pack . userpassPassword $ up))

-------------------------------------------------------------------------------
cfgToUserPass
    :: [(Text, CT.Value)] -- ^ Pairs of configuration values extracted from the application's configuration file
    -> UserPass -- ^ A UserPass backend for a particular HTTPAuth domain.
cfgToUserPass cfg =
    let
        u = cfgLookupWithDefault "Username" "" stringValue cfg
        p = cfgLookupWithDefault "Password" "" stringValue cfg
        in
            UserPass u p
