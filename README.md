# HTTP Auth Snaplet for Snap Framework

[![Build Status](https://travis-ci.org/anchor/snaplet-httpauth.svg)](https://travis-ci.org/anchor/snaplet-httpauth)

This Snaplet enables you to use HTTP Auth with configurable backends.

© Anchor Systems 2014

Note that this Snaplet is still under development, and the documentation below is still provisional.

## Installation

```shell
cabal install snaplet-httpauth
```

## Usage

### Configuration

In your Snap application's devel.cfg file, or whatever configuration is appropriate for your environment, you'll be assembling AuthDomain sections that describe which type of authentication is appropriate for which domain.

Here's an example:

```text
AuthDomains
{
    display
    {
        AuthenticationType = "AllowEverything"
    }
    generalPublic
    {
        AuthenticationType = "AllowEverything"
    }
    securedArea
    {
        AuthenticationType = "HypotheticalAPI"
        OwnUserURL = "https://example.com/currentUser"
        OwnUserMethod = "get"
        DefaultRoles = ["Admin"]
    }
}
```

This will define three authentication domains, which will all prompt for credentials anew.

We recommend that you implement at least one domain that's used only for displaying things at the very least, so that your Heist splices will work.

### Application Definition

In the definition of your `App` data type in Application.hs, you'll need to add a parameter for AuthConfig. Here's an example:

```haskell
data App = App
    { _heist    :: Snaplet (Heist App)
    , _sess     :: Snaplet SessionManager
    , _httpauth :: Snaplet AuthConfig
    }
```

### Application Setup

In the definition of `app` in Site.hs, you'll need to configure it something like the following:

```haskell
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do

    h <- nestSnaplet "" heist $ heistInit' "templates" hc
    setInterpreted h

    s <- nestSnaplet "sess" sess $
         initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    cfg <- getSnapletUserConfig

    let authHeaders = [parserToAHW parseBasicAuthHeader]
    let authTypes = [("AllowEverything", configToADT cfgToAllowEverything)]

    ac <- liftIO $ getAuthManagerCfg authHeaders authTypes cfg
    a <- nestSnaplet "httpauth" httpauth $ authInit ac

    addHTTPAuthSplices h httpauth "display"
    addRoutes routes

    return $ App h s a ss rc
```

Note how `getAuthManagerCfg` takes three different configuration objects:

* Definitions for the type of `Authorization` header that your app accepts
* Definitions for the type of backend that your app supports
* The actual Data.Configurator.Types.Config object that will be used to configure the Snaplet

### Usage in Handlers

Now, you'll be able to define your handlers like so:

```haskell
printSomething :: Handler App App ()
printSomething = withAuth "generalPublic" httpauth $ do
    writeBS . C.pack $ "Hello world!"

printSomethingSecret :: Handler App App ()
printSomethingSecret = withAuth "securedArea" httpauth $ do
    writeBS . C.pack $ "Hello, I'm a secret :3"
```

Note that the domain name used in withAuth directly maps to the domain names in your configuration file.

## Extending

This section is yet to be populated, but will contain details of how to build your own Authorization header parsers and HTTPAuth backends.


