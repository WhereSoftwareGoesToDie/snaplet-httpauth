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
class AuthHeader r where
    authHeaderType  :: r -> String
    authHeaderField :: r -> String -> Maybe String
    toHeader        :: r -> ByteString

------------------------------------------------------------------------
data AuthHeaderWrapper = AuthHeaderWrapper (String, String -> Maybe String, ByteString)

parserToAHW
    :: (AuthHeader r)
    => (ByteString -> Maybe r)
    -> ByteString
    -> Maybe AuthHeaderWrapper
parserToAHW parser headerStr = translateOK <$> parser headerStr
  where
    translateOK x = AuthHeaderWrapper (authHeaderType x, authHeaderField x, toHeader x)

parseAuthorizationHeader
    :: [ByteString -> Maybe AuthHeaderWrapper]
    -> Maybe ByteString
    -> Maybe AuthHeaderWrapper
parseAuthorizationHeader _ Nothing = Nothing
parseAuthorizationHeader parsers (Just headerStr) = headMay . catMaybes $ [ fn headerStr | fn <- parsers ]
