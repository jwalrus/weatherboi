module Lib (
      buildHttpRequest
    , buildHttpsRequest
    , weatherHost
    ) where

import qualified Data.ByteString.Char8 as BC
import           Data.List (stripPrefix)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status

weatherHost :: T.Text
weatherHost = "api.weather.gov"

userAgent :: BC.ByteString
userAgent = "WeatherBoi/v0.1 (http://github.com/jwalrus/weatherboi; jwalrus@protonmail.com)"

buildHttpRequest :: T.Text -> T.Text -> T.Text -> Request
buildHttpRequest method host path = setRequestMethod (encodeUtf8 method)
                                  $ setRequestHost (encodeUtf8 host)
                                  $ setRequestPath (encodeUtf8 path)
                                  $ setRequestHeaders [("User-Agent", userAgent), ("Accept", "application/ld+json")]
                                  $ defaultRequest  

buildHttpsRequest :: T.Text -> T.Text -> T.Text -> Request
buildHttpsRequest method host path = setRequestMethod (encodeUtf8 method)
                                   $ setRequestHost (encodeUtf8 host)
                                   $ setRequestPath (encodeUtf8 path)
                                   $ setRequestHeaders [("User-Agent", userAgent), ("Accept", "application/ld+json")]
                                   $ setRequestSecure True
                                   $ setRequestPort 443
                                   $ defaultRequest

