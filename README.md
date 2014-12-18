# HTTP Auth Snaplet for Snap Framework

[![Build Status](https://travis-ci.org/anchor/snaplet-httpauth.svg)](https://travis-ci.org/anchor/snaplet-httpauth)

This Snaplet enables you to use HTTP Auth with configurable backends.

© Anchor Systems 2014

Note that this Snaplet is still under development, and the documentation below is still provisional.

## Installation

```shell
cabal install snaplet-httpauth
```

## Usage in simple Snap applications

To use the HTTPAuth mechanism alone in a simple Snap application, without requiring the additional Snaplet framework, configuration handling and Heist, you can define your authentication configuration and invoke `withAuthDomain` on its own.

Here's a quick example:

```haskell
import Snap.Snaplet.HTTPAuth

myHandler :: Snap ()
myHandler = withAuthDomain [] defaultAuthHeaders myDomain $
    writeBS "Hello world"
  where
    myDomain = AuthDomain "testdomain" (wrapDataSource $ UserPass "foo" "bar")
```

In this example, we're providing the following arguments to `withAuthDomain`:

* A list of additional roles that are relevant to this particular resource, which will be evaluated by the `validateUser` call in `AuthDataWrapper`. Since this resource isn't particularly special, we left it blank.
* A list of methods that are able to parse an Authorization header, that will be evaluated in turn until we get one that works. `defaultAuthHeaders` implements Basic headers only.
* An `AuthDomain`, prepared with a wrapped source value that implements the class `IAuthDataSource`.

## Usage as a Snaplet

To use the HTTPAuth snaplet as a snaplet, within an application, you'll need to make sure to include the right modules as part of your Site and App declarations, as well as anything that calls HTTPAuth methods.

```haskell
import Snap.Snaplet.HTTPAuth
import Snap.Snaplet.HTTPAuth.Application
import Snap.Snaplet.HTTPAuth.Heist
```

### Configuration

In your Snap application's devel.cfg file, or whatever configuration is appropriate for your environment, you'll be assembling AuthDomain sections that describe which type of authentication is appropriate for which domain.

Here's an example:

```text
AuthDomains
{
    display
    {
        AuthenticationType = "IfHeader"
    }
    requiresHeader
    {
        AuthenticationType = "IfHeader"
    }
    requiresSpecificUser
    {
        AuthenticationType = "UserPass"
        Username = "foo"
        Password = "bar"
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

This will define four authentication domains, which will all prompt for credentials.

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

In the definition of `app` in Site.hs, you'll need to invoke your snaplet like this, if you want the default (bare bones) configuration.

```haskell
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do

    h <- nestSnaplet "" heist $ heistInit' "templates" hc
    setInterpreted h

    s <- nestSnaplet "sess" sess $
         initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    cfg <- getSnapletUserConfig
    ac <- liftIO $ getAuthManagerCfg defaultAuthHeaders defaultAuthDomains cfg
    a <- nestSnaplet "httpauth" httpauth $ authInit ac

    addHTTPAuthSplices h httpauth "display"
    addRoutes routes

    return $ App h s a
```

If you want to use custom sets of AuthHeader parsers and HTTPAuth backends, you'll modify it slightly and use something like this:

```haskell
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do

    h <- nestSnaplet "" heist $ heistInit' "templates" hc
    setInterpreted h

    s <- nestSnaplet "sess" sess $
         initCookieSessionManager "site_key.txt" "sess" (Just 3600)

    cfg <- getSnapletUserConfig

    let authHeaders = [ parserToAHW parseBasicAuthHeader
                      , parserToAHW parseCustomAuthHeader ]
    let authDomains = [ ("IfHeader", configToADT cfgToAllowEverythingIfHeader)
                      , ("UserPass", configToADT cfgToUserPass)
                      , ("HypotheticalAPI", configToADT cfgToHypotheticalAPI) ]

    ac <- liftIO $ getAuthManagerCfg authHeaders authDomains cfg
    a <- nestSnaplet "httpauth" httpauth $ authInit ac

    addHTTPAuthSplices h httpauth "display"
    addRoutes routes

    return $ App h s a
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

Here are details on how to write your own HTTPAuth backend. Details of how to build your own Authorization header parsers are yet to come.

### Writing your own HTTPAuth Backend

A HTTPAuth backend is comprised of three parts:

* a data type to contain configuration for the backend you are connecting to
* an instance of IAuthDataSource for your data type, with implementations of its `getUser` and `validateUser` methods
* a function to convert configuration data from the site's configuration file, expressed as `[(Text, Data.Configurator.Types.Value)]`, to your data type

As an example, we'll look at the UserPass backend, which stores a single username and password, and authenticates against them.

The data type in this case is pretty simple:

```haskell
data UserPass = UserPass {
    userpassUsername :: String,
    userpassPassword :: String
}
```

Similarly, the function to convert configuration data to a UserPass is also pretty simple.

```haskell
import qualified Data.Configurator.Types as CT
-- put the import above at top of module

cfgToUserPass
    :: [(Text, CT.Value)] -- ^ Pairs of configuration values extracted from the application's configuration file
    -> UserPass -- ^ A UserPass backend for a particular HTTPAuth domain.
cfgToUserPass cfg =
    let
        u = cfgLookupWithDefault "Username" "" stringValue cfg
        p = cfgLookupWithDefault "Password" "" stringValue cfg
        in
            UserPass u p
```

As you can see, we're just pulling the Username and Password keys from the configuration, converting them to strings if they're found, and then passing them as parameters to the UserPass object.

(`cfgLookupWithDefault` is a helper method from [snap-configuration-utilities](http://hackage.haskell.org/package/snap-configuration-utilities), that takes a key to look up, a default value to set, and a method to extract the value if found, and applies them to the configuration.)

The instance for IAuthDataSource is a little more complex.

```haskell
import qualified Data.ByteString.Char8 as C
-- put the import above at top of module

instance IAuthDataSource UserPass where
    getUser up (AuthHeaderWrapper (_,gf,_)) = return $
        if gf "Username" == (Just . userpassUsername $ up)
            then Just $
                AuthUser (C.pack . userpassUsername $ up)
                         (fromList withPasswd)
            else Nothing
      where
        withPasswd = case gf "Password" of
            Just p  -> [("Password", C.pack p)]
            Nothing -> []
    validateUser up _ (AuthUser username f) =
        (username == (C.pack . userpassUsername $ up)) &&
        (lookup "Password" f == (Just . C.pack . userpassPassword $ up))
```

In this case, `getUser` takes a UserPass and an AuthHeaderWrapper.

For the AuthHeaderWrapper, we use pattern matching to get access to its component methods. We only need the 2nd item, to get at the Authorization header's fields.

We check to see if the provided username matches the one in the UserPass object. If it does, we return an AuthUser object, with the provided username, but we set its password to the one from the Authorization header. This makes sure that `validateUser` will work correctly.

`validateUser` validates a user extracted from Authorization headers against your IAuthDataSource object and a list of extra roles which your handler can optionally pass to you. UserPass has no concept of roles, so we're ignoring those in this case.

The implementation of `validateUser` is pretty simple. It validates the user if the provided username matches the username in the UserPass object, **and** if the provided password matches the password in the UserPass object.

### Writing your own Authorization header parsers

An Authorization header parser is comprised of three components:

* a data type to contain information parsed out of Authorization headers
* an instance of class AuthHeader for your data type, with implementations of its `authHeaderType`, `authHeaderField` and `toHeader` methods
* the actual parser method, with type signature `ByteString -> Maybe YourCustomHeaderType`

As an example, we'll look at the BasicAuthHeader parser, which parses Authorization headers with the Basic authentication type.

The data type is pretty simple:

```haskell
data BasicAuthHeader = BasicAuthHeader (Map String String)
```

We're only going to store two keys in here, Username and Password. It might be more efficient not to use a Map, but this model is adaptable to other Authorization header types that contain more information.

The instance of AuthHeader is also pretty simple:

```haskell
import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromMaybe)
-- put the import above at top of module

instance AuthHeader BasicAuthHeader where
    authHeaderType _ = "BasicAuth"
    authHeaderField (BasicAuthHeader m) f = lookup f m
    toHeader x = C.concat [C.pack "Basic ", B64.encode $ C.pack (f "Username" ++ ":" ++ f "Password")]
      where
        f = fromMaybe "" . authHeaderField x
```

The `authHeaderType` gives us a String representing what kind of header this is, so HTTPAuth backends can identify them if needed. This is required because we're going to be packing all these methods into `AuthHeaderWrapper` objects, which are generic in nature and hide the AuthHeader object within.

`authHeaderField` just looks up the field in this header matching a particular String key. It returns a Maybe String, meaning that it's valid to not return anything.

`toHeader` reconstitutes the header back into a ByteString that can be passed to another HTTP request, which is necessary if we need to use it for authentication against other services. As you can see, we're fetching the Username and Password fields, packing them up with a colon separator and base64 encoding the lot, before dropping Basic on the front, as per the [Basic Authentication specification](http://tools.ietf.org/html/rfc2617).

The parser is a little more complex, but you should be able to follow how it works.

```haskell
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
```

First, we're checking to make sure that the header starts with the text "Basic " – we're splitting on the separating space to determine that this is the case. Since spaces are not part of base64 encoding, we can work on decoding the next chunk.

If we can decode the base64-encoded chunk, we can then split on the separating colon, fetch the username and password, unpack both, and return them as part of our BasicAuthHeader object.

You might need to do some more complex parsing code for things like token-based authentication headers, but tools like Attoparsec/Trifecta would be suitable for extracting data from those.
