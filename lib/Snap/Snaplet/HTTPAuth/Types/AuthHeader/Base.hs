{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeFamilies              #-}

module Snap.Snaplet.HTTPAuth.Types.AuthHeader.Base where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Maybe
import Safe

-------------------------------------------------------------------------------
-- | Basic class definition for all Authorization header parsers.
class AuthHeader r where
    authHeaderType  :: r -> String
    authHeaderField :: r -> String -> Maybe String
    toHeader        :: r -> ByteString

------------------------------------------------------------------------
-- | A wrapper around an object that implements the AuthHeader class.
-- Contains a tuple of three functions:
-- a function that returns the type of header as a String,
-- a function that extracts an individual field from the header (such as Username or Password or whatever),
-- and a function that reconstitutes the header into a ByteString, ready to be used in HTTP requests.
data AuthHeaderWrapper = AuthHeaderWrapper (String, String -> Maybe String, ByteString)

-- | Builds a potential AuthHeaderWrapper out of configuration information.
-- Most often used as a partial method when setting up your site, so
-- that you have a list of functions to pass to `parseAuthorizationHeader`.
parserToAHW
    :: (AuthHeader r)
    => (ByteString -> Maybe r) -- ^ Function that converts an Authorization header into an object of class AuthHeader.
    -> ByteString -- ^ The Authorization header to parse.
    -> Maybe AuthHeaderWrapper -- ^ A potential internal representation of the header, that can be passed to an authentication backend.
parserToAHW parser headerStr = translateOK <$> parser headerStr
  where
    translateOK x = AuthHeaderWrapper (authHeaderType x, authHeaderField x, toHeader x)

-- | Parses an Authorization HTTP header against a list of recognised
-- header definitions.
parseAuthorizationHeader
    :: [ByteString -> Maybe AuthHeaderWrapper] -- ^ A list of recognised Authorization header parsers.
    -> Maybe ByteString -- ^ The Authorization header to parse.
    -> Maybe AuthHeaderWrapper -- ^ A potential internal representation of the header, that can be passed to an authentication backend.
parseAuthorizationHeader _ Nothing = Nothing
parseAuthorizationHeader parsers (Just headerStr) = headMay . catMaybes $ [ fn headerStr | fn <- parsers ]
