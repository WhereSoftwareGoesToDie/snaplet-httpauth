{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeFamilies              #-}

module Snap.Snaplet.HTTPAuth.Types.AuthHeader.Basic where

import Prelude hiding (lookup)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C
import Data.Map
import Data.Maybe
import Safe

import Snap.Snaplet.HTTPAuth.Types.AuthHeader.Base

-------------------------------------------------------------------------------
data BasicAuthHeader = BasicAuthHeader (Map String String)

instance AuthHeader BasicAuthHeader where
    authHeaderType _ = "BasicAuth"
    authHeaderField (BasicAuthHeader m) f = lookup f m
    toHeader x = C.concat [C.pack "Basic ", B64.encode $ C.pack (f "Username" ++ ":" ++ f "Password")]
      where
        f = maybe "" id . authHeaderField x

parseBasicAuthHeader
    :: ByteString
    -> Maybe BasicAuthHeader
parseBasicAuthHeader x = case (C.split ' ' x) of
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
