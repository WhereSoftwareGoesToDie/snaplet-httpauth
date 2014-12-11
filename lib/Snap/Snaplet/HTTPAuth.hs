{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}

module Snap.Snaplet.HTTPAuth (
    AuthConfig (..),
    AuthDomain (..),

    getAuthManagerCfg,

    authInit,
    withAuth,
    addHTTPAuthSplices,

    AuthHeaderWrapper (..),
    parseBasicAuthHeader,
    parserToAHW,
    parseAuthorizationHeader,

    configToADT,
    cfgToAllowEverything,
    cfgToAllowEverythingIfHeader,
    cfgToUserPass
) where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Monoid
import Data.Text.Encoding (decodeUtf8)
import Heist
import qualified Text.XmlHtml as X

import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.HTTPAuth.App
import Snap.Snaplet.HTTPAuth.Backend
import Snap.Snaplet.HTTPAuth.Config
import Snap.Snaplet.HTTPAuth.Types

--------------------------------------------------------------------------------
-- | Initialise Auth snaplet
authInit :: AuthConfig -> SnapletInit b AuthConfig
authInit cfg = makeSnaplet "auth" "Handles user authentication" Nothing $ return cfg

--------------------------------------------------------------------------------
-- | Splices for CSS and JS
addHTTPAuthSplices
    :: HasHeist b
    => Snaplet (Heist b)
    -> SnapletLens b AuthConfig
    -> String
    -> Initializer b v ()
addHTTPAuthSplices h auth domainName = addConfig h sc
  where
    sc = mempty & scInterpretedSplices .~ intSpli
    intSpli = "currentUser" ## authCurrentUser
    authCurrentUser = lift $
        liftM (replicate 1 . X.TextNode . maybe "Nobody" (decodeUtf8 . authUserIdentity))
              (currentUserInDomain domainName auth)
