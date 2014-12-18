{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.HTTPAuth.Heist (
    addHTTPAuthSplices
) where

import Control.Applicative
import Control.Lens
import Control.Monad.State
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Monoid
import Data.Text.Encoding (decodeUtf8)
import Heist
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import qualified Text.XmlHtml as X

import Snap.Snaplet.HTTPAuth.Application
import Snap.Snaplet.HTTPAuth.Authorise
import Snap.Snaplet.HTTPAuth.Backend.Utilities.Configurator
import Snap.Snaplet.HTTPAuth.Config
import Snap.Snaplet.HTTPAuth.Types

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
