{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

module Simple.App where

import Control.Applicative
import Control.Lens
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Snap.Core
import Snap.Http.Server
import Snap.Snaplet
import Snap.Snaplet.HTTPAuth
import Snap.Snaplet.HTTPAuth.Backend.UserPass
import Snap.Snaplet.HTTPAuth.Types.IAuthDataSource
import Snap.Util.FileServe

site :: Snap ()
site =
    ifTop (writeBS "hello world") <|>
    route [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          ] <|>
    dir "static" (serveDirectory ".")

echoHandler :: Snap ()
echoHandler = withAuthDomain [] defaultAuthHeaders simpleAuthDomain $ do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param

simpleAuthDomain :: AuthDomain
simpleAuthDomain = AuthDomain "testdomain" (wrapDataSource $ UserPass "foo" "bar")
