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

-- * Extending

{-$ Here are details on how to write your own HTTPAuth backend. Details of how to build your own Authorization header parsers are yet to come. -}

-- ** Writing your own HTTPAuth Backend

{-$

    A HTTPAuth backend is comprised of three parts:

        * a data type to contain configuration for the backend you are connecting to
        * an instance of IAuthDataSource for your data type, with implementations of its `getUser` and `validateUser` methods
        * a function to convert configuration data from the site's configuration file, expressed as `[(Text, Data.Configurator.Types.Value)]`, to your data type

    As an example, we'll look at the UserPass backend, which stores a single username and password, and authenticates against them.

    The data type in this case is pretty simple:

    > data UserPass = UserPass {
    >     userpassUsername :: String,
    >     userpassPassword :: String
    > }

    Similarly, the function to convert configuration data to a UserPass is also pretty simple.

    >     import qualified Data.Configurator.Types as CT
    >     -- put the import above at top of module
    > 
    >     cfgToUserPass
    >         :: [(Text, CT.Value)] -- ^ Pairs of configuration values extracted from the application's configuration file
    >         -> UserPass -- ^ A UserPass backend for a particular HTTPAuth domain.
    >     cfgToUserPass cfg =
    >         let
    >             u = cfgLookupWithDefault "Username" "" stringValue cfg
    >             p = cfgLookupWithDefault "Password" "" stringValue cfg
    >             in
    >                 UserPass u p

    As you can see, we're just pulling the Username and Password keys from the configuration, converting them to strings if they're found, and then passing them as parameters to the UserPass object.

    (`cfgLookupWithDefault` is a helper method from "Snap.Utilities.Configuration", that takes a key to look up, a default value to set, and a method to extract the value if found, and applies them to the configuration.)

    The instance for IAuthDataSource is a little more complex.

    > import qualified Data.ByteString.Char8 as C
    > -- put the import above at top of module
    > 
    > instance IAuthDataSource UserPass where
    >     getUser _ Nothing  = return Nothing
    >     getUser up (Just (AuthHeaderWrapper (_,gf,_))) = return $
    >         if gf "Username" == (Just . userpassUsername $ up)
    >             then Just $
    >                 AuthUser (C.pack . userpassUsername $ up)
    >                          (fromList withPasswd)
    >             else Nothing
    >       where
    >         withPasswd = case gf "Password" of
    >             Just p  -> [("Password", C.pack p)]
    >             Nothing -> []
    >     validateUser up _ (AuthUser username f) =
    >         (username == (C.pack . userpassUsername $ up)) &&
    >         (lookup "Password" f == (Just . C.pack . userpassPassword $ up))

    `getUser` takes a UserPass object and a Maybe AuthHeaderWrapper. If we receive Nothing for the latter, we didn't successfully parse an Authorization header, and therefore we have no user to return.

    If we got a Just AuthHeaderWrapper, we use pattern matching to get access to its component methods. We only need the 2nd item, to get at the Authorization header's fields.

    We check to see if the provided username matches the one in the UserPass object. If it does, we return an AuthUser object, with the provided username, but we set its password to the one from the Authorization header. This makes sure that `validateUser` will work correctly.

    `validateUser` validates a user extracted from Authorization headers against your IAuthDataSource object and a list of extra roles which your handler can optionally pass to you. UserPass has no concept of roles, so we're ignoring those in this case.

    The implementation of `validateUser` is pretty simple. It validates the user if the provided username matches the username in the UserPass object, **and** if the provided password matches the password in the UserPass object.

-}