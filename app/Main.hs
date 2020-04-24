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
import           Points
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

printResults :: Either String WeatherResponse -> IO ()
printResults (Left errorMsg) = print $ "error loading data: " ++ errorMsg
printResults (Right results) = do
    -- print results
    print $ forecast results
    let forecastPath = BC.stripPrefix (BC.append "https://" weatherHost) $ (encodeUtf8 . forecast) results
    case forecastPath of
        Nothing -> print "failed to find forecast"
        Just (path) -> do
            print $ forecastHourly results
            response <- httpLBS $ buildRequest weatherHost "GET" path userAgent
            let (Status code message) = getResponseStatus response
            if code == 200
                then do
                    let jsonBody = getResponseBody response
                    let forecastResponse = eitherDecode jsonBody :: Either String F.Forecast
                    case forecastResponse of
                      Left (err) -> print $ "Failed to decode forecast: " ++ err
                      Right (f) -> print $ "Forecast: " ++ show f
                    L.writeFile "forecast.json" jsonBody
                    print "saved forecast data"
                else do
                    print "failed forecast request"
                    print $ "error code: " ++ show code
                    print $ "error msg: " ++ show message


main :: IO ()
main = do
    response <- httpLBS request
    let (Status code message) = getResponseStatus response
    if code == 200
        then do
            let jsonBody = getResponseBody response
            let weatherResponse = eitherDecode jsonBody :: Either String WeatherResponse
            printResults weatherResponse 
            print "saved response to data.json"
            L.writeFile "data.json" jsonBody
        else do
            print "failed request"
            print $ "error code: " ++ show code
            print $ "error message: " ++ show message
    r2 <- LL.fetchLatLong LL.latLongRequest
    print r2     


