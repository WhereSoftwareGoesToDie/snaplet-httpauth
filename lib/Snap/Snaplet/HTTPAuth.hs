module Snap.Snaplet.HTTPAuth (
    AuthConfig (..),
    AuthDomain (..),

    IAuthDataSource (..),

    parseBasicAuthHeader,
    parserToAHW,
    parseAuthorizationHeader,

    defaultAuthHeaders,
    defaultAuthDomains,

    withAuthDomain
) where

import Snap.Snaplet.HTTPAuth.Authorise
import Snap.Snaplet.HTTPAuth.Backend
import Snap.Snaplet.HTTPAuth.Types
