module Snap.Snaplet.HTTPAuth (
    AuthConfig (..),
    AuthDomain (..),

    IAuthDataSource (..),
    wrapDataSource,

    parseBasicAuthHeader,
    parserToAHW,
    parseAuthorizationHeader,

    defaultAuthHeaders,

    withAuthDomain,
    withAuthDomain'
) where

import Snap.Snaplet.HTTPAuth.Authorise
import Snap.Snaplet.HTTPAuth.Backend
import Snap.Snaplet.HTTPAuth.Types
