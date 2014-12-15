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
    cfgToUserPass,

    defaultAuthHeaders,
    defaultAuthDomains
) where

import Control.Applicative
import Control.Lens
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
-- | Initialise HTTPAuth snaplet.
authInit
    :: AuthConfig               -- ^ Configuration
    -> SnapletInit b AuthConfig -- ^ The initialised HTTPAuth Snaplet
authInit =
    makeSnaplet "auth" "Handles user authentication" Nothing . return

--------------------------------------------------------------------------------
-- | Generate splices for the HTTPAuth snaplet.
-- This allows the use of the `currentUser` tag to display the name of the current user.
addHTTPAuthSplices
    :: HasHeist b
    => Snaplet (Heist b)        -- ^ Heist Snaplet
    -> SnapletLens b AuthConfig -- ^ Lens to this application's AuthConfig
    -> String                   -- ^ HTTPAuth domain name matching one of the
                                --   domains defined in the AuthDomains config
    -> Initializer b v ()
addHTTPAuthSplices h auth domain_name = addConfig h sc
  where
    sc = mempty & scInterpretedSplices .~ ("currentUser" ## authCurrentUser)
    authCurrentUser = lift $ do
        u <- maybe "Nobody" (decodeUtf8 . authUserIdentity)
                <$> currentUserInDomain domain_name auth
        return [X.TextNode u]
