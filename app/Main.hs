module Main where

import           Control.Monad (forM_)
import           Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import           Data.List (stripPrefix)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)
import           GHC.Generics
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status
import           Points as P
import qualified Forecast as F
import qualified GeoNamesLatLong as LL


apiPath :: BC.ByteString
apiPath = "/points/39.5,-98.35"

weatherHost :: BC.ByteString
weatherHost = "api.weather.gov"

userAgent :: BC.ByteString
userAgent = "WeatherBoi/v0.1 (http://github.com/jwalrus/weatherboi; jwalrus@protonmail.com)"

buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString 
             -> BC.ByteString -> Request
buildRequest host method path ua = setRequestMethod method
                                    $ setRequestHost host
                                    $ setRequestHeaders [("User-Agent", ua), ("Accept", "application/ld+json")]
                                    $ setRequestPath path
                                    $ setRequestSecure True
                                    $ setRequestPort 443
                                    $ defaultRequest

request :: Request
request = buildRequest weatherHost "GET" apiPath userAgent


main :: IO ()
main = do
    -- todo: get address, state, zip from user and use to construct lat/lng request
    -- todo: need better way to construct requests
    -- todo: need to pass lat lng into next request rather than hard-coding
    -- todo: need to move buildRequest and other stuff into Lib
    latLng <- LL.fetchLatLong LL.latLongRequest
    case latLng of
        Nothing -> print "Failed to get latitude / longitude for request"
        Just (LL.LatLong lat lng) -> do
            print $ "latitude = " ++ show lat ++ " / longitude = " ++ show lng
            weatherPoints <- P.fetchWeatherPoints request
            case weatherPoints of
                Nothing -> print "Failed to get points data from national weather service"
                Just (weatherResponse) -> do
                    F.fetchForecast weatherResponse

