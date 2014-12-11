{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeFamilies              #-}

module Snap.Snaplet.HTTPAuth.Types.AuthHeader.Basic where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C
import Data.Map
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)

import Snap.Snaplet.HTTPAuth.Types.AuthHeader.Base

-------------------------------------------------------------------------------
-- | Internal representation of Basic authorization header.
-- The Map in this object contains two key/value pairs: Username and Password.
data BasicAuthHeader = BasicAuthHeader (Map String String)

instance AuthHeader BasicAuthHeader where
    authHeaderType _ = "BasicAuth"
    authHeaderField (BasicAuthHeader m) f = lookup f m
    toHeader x = C.concat [C.pack "Basic ", B64.encode $ C.pack (f "Username" ++ ":" ++ f "Password")]
      where
        f = fromMaybe "" . authHeaderField x

-- | Parses a Basic Authorization header into an internal object that contains its details.
parseBasicAuthHeader
    :: ByteString
    -> Maybe BasicAuthHeader
parseBasicAuthHeader x = case C.split ' ' x of
    ("Basic":x':_) ->
        case B64.decode x' of
            Left _ -> Nothing
            Right x'' ->
                case C.split ':' x'' of
                    (u:p:_) -> let
                        hMap = fromList [("Username", C.unpack u),
                                         ("Password", C.unpack p)]
                        in Just $ BasicAuthHeader hMap
                    _ -> Nothing
    _ -> Nothing
