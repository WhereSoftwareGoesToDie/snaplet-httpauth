{-# LANGUAGE OverloadedStrings, RecordWildCards, TemplateHaskell #-}

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
import Control.Monad.State
import Control.Monad.Trans.Class
import qualified Data.ByteString.Char8 as C
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Heist
import Heist.Splices
import qualified Heist.Interpreted as I
import qualified Text.XmlHtml as X

import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.HTTPAuth.Backend
import Snap.Snaplet.HTTPAuth.Config
import Snap.Snaplet.HTTPAuth.Types
import Snap.Snaplet.HTTPAuth.App

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
    authCurrentUser =
        lift $ currentUserInDomain domainName auth >>=
        return . take 1 . repeat . X.TextNode . maybe "Nobody" (decodeUtf8 . authUserIdentity)
