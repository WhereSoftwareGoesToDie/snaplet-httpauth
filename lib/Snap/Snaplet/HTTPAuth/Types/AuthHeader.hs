{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE ImpredicativeTypes        #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeFamilies              #-}

module Snap.Snaplet.HTTPAuth.Types.AuthHeader (
    AuthHeaderWrapper (..),
    parseBasicAuthHeader,
    parserToAHW,
    parseAuthorizationHeader
) where

import Snap.Snaplet.HTTPAuth.Types.AuthHeader.Base
import Snap.Snaplet.HTTPAuth.Types.AuthHeader.Basic
