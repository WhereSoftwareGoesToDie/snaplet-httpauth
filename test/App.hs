{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module App where

import Control.Lens
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Configurator as C
import qualified Data.Configurator.Types as CT
import Data.Monoid
import qualified Data.Text as T
import Heist
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Heist
import Snap.Snaplet.HTTPAuth
import Snap.Util.FileServe

------------------------------------------------------------------------------
-- App

data App = App { _heist :: Snaplet (Heist App), _httpauth :: Snaplet AuthConfig }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

type AppHandler = Handler App App

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("tpl",         publicTpl)
         , ("domain1",     domain1Page)
         , ("domain2",     domain2Page)
         , ("pub",         publicPage) ]

-- | A public page, with no HTTP Auth
publicPage :: Handler App App ()
publicPage = writeBS . C8.pack $ "Hello world"

-- | A private page in domain 1
domain1Page :: Handler App App ()
domain1Page = withAuth "domain1" httpauth $ writeBS . C8.pack $ "Hello domain1"

-- | A private page in domain 2
domain2Page :: Handler App App ()
domain2Page = withAuth "domain2" httpauth $ writeBS . C8.pack $ "Hello domain2"

-- | A public tpl page
publicTpl :: Handler App App ()
publicTpl = render "snaplet_httpauth_test"

-- | A private tpl page in domain 2
domain1Tpl :: Handler App App ()
domain1Tpl = withAuth "domain1" httpauth $ render "snaplet_httpauth_test"

-- | A private tpl page in domain 2
domain2Tpl :: Handler App App ()
domain2Tpl = withAuth "domain2" httpauth $ render "snaplet_httpauth_test"

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "Test app" Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    setInterpreted h
    
    cfg <- getSnapletUserConfig

    let authHeaders = [parserToAHW parseBasicAuthHeader]
    let authTypes = [ ("AllowEverything", configToADT cfgToAllowEverything)
                    , ("IfHeader",        configToADT cfgToAllowEverythingIfHeader)
                    , ("UserPass",        configToADT cfgToUserPass)]

    ac <- liftIO $ getAuthManagerCfg authHeaders authTypes cfg
    a <- nestSnaplet "httpauth" httpauth $ authInit ac

    addHTTPAuthSplices h httpauth "display"
    addRoutes routes

    return $ App h a
