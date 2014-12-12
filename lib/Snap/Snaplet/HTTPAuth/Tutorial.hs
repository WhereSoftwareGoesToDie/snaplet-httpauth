{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Snap.Snaplet.HTTPAuth.Tutorial where

{-$ This Snaplet enables you to use HTTP Auth with configurable backends. -}

-- * Configuration

{-$ In your Snap application's devel.cfg file, or whatever configuration is appropriate for your environment, you'll be assembling AuthDomain sections that describe which type of authentication is appropriate for which domain.

    Here's an example:

    > AuthDomains
    > {
    >     display
    >     {
    >         AuthenticationType = "AllowEverything"
    >     }
    >     generalPublic
    >     {
    >         AuthenticationType = "AllowEverything"
    >     }
    >     securedArea
    >     {
    >         AuthenticationType = "HypotheticalAPI"
    >         OwnUserURL = "https://example.com/currentUser"
    >         OwnUserMethod = "get"
    >         DefaultRoles = ["Admin"]
    >     }
    > }

    This will define three authentication domains, which will all prompt for credentials anew.

    We recommend that you implement at least one domain that's used only for displaying things at the very least, so that your Heist splices will work.

 -}

-- * Application Definition

{-$ In the definition of your `App` data type in Application.hs, you'll need to add a parameter for AuthConfig. Here's an example:

    > data App = App
    >     { _heist    :: Snaplet (Heist App)
    >     , _sess     :: Snaplet SessionManager
    >     , _httpauth :: Snaplet AuthConfig
    >     }

 -}

-- * Application Setup

{-$ In the definition of `app` in Site.hs, you'll need to configure it something like the following:

    > app :: SnapletInit App App
    > app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    >
    >     h <- nestSnaplet "" heist $ heistInit' "templates" hc
    >     setInterpreted h
    >
    >     s <- nestSnaplet "sess" sess $
    >          initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    >
    >     cfg <- getSnapletUserConfig
    >
    >     let authHeaders = [parserToAHW parseBasicAuthHeader]
    >     let authTypes = [("AllowEverything", configToADT cfgToAllowEverything)]
    >
    >     ac <- liftIO $ getAuthManagerCfg authHeaders authTypes cfg
    >     a <- nestSnaplet "httpauth" httpauth $ authInit ac
    >
    >     addHTTPAuthSplices h httpauth "display"
    >     addRoutes routes
    >
    >     return $ App h s a ss rc

    Note how `getAuthManagerCfg` takes three different configuration objects:

        * Definitions for the type of `Authorization` header that your app accepts
        * Definitions for the type of backend that your app supports
        * The actual Data.Configurator.Types.Config object that will be used to configure the Snaplet

 -}

-- * Usage in Handlers

{-$ Now, you'll be able to define your handlers like so:

    > printSomething :: Handler App App ()
    > printSomething = withAuth "generalPublic" httpauth $ do
    >     writeBS . C.pack $ "Hello world!"
    >
    > printSomethingSecret :: Handler App App ()
    > printSomethingSecret = withAuth "securedArea" httpauth $ do
    >     writeBS . C.pack $ "Hello, I'm a secret :3"

    Note that the domain name used in withAuth directly maps to the domain names in your configuration file.

-}
