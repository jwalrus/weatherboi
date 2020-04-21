module Main where

import           Control.Monad (forM_)
import           Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Text as T
import           GHC.Generics
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status
import           Lib

type URL = String

data Geometry  = Geometry {
      geometryJsonType :: String
    , coordinates :: (Double, Double)
    } deriving (Show)

instance FromJSON Geometry where
    parseJSON (Object v) =
        Geometry <$> v .: "type"
                 <*> v .: "coordinates"

data Distance = Distance {
      distanceValue :: Double
    , distanceUnitCode :: String
    } deriving (Show)

instance FromJSON Distance where
    parseJSON (Object v) =
        Distance <$> v .: "value"
                 <*> v .: "unitCode"

data Bearing = Bearing {
      bearingValue :: Int
    , bearingUnitCode :: String
    } deriving (Show)

instance FromJSON Bearing where
    parseJSON (Object v) =
        Bearing <$> v .: "value"
                <*> v .: "unitCode"

data Location = Location {
      city :: String
    , state :: String
    , distance :: Distance
    , bearing :: Bearing
    } deriving (Show, Generic)

instance FromJSON Location

--data Location = Location {
--      locationJsonType :: String
--    , locationGeometry :: String
--    , locationProperties :: LocationProperties
--    } deriving (Show)

--instance FromJSON Location where
--    parseJSON (Object v) =
--        Location <$> v .: "type"
--                 <*> v .: "geometry"
--                 <*> v .: "properties"

--data WeatherProperties = WeatherProperties {
--      atId :: URL
--    , atType :: String
--    , cwa :: String
--    , forecastOffice :: URL
--    , gridX :: Int
--    , gridY :: Int
--    , forecast :: URL
--    , forecastHourly :: URL
--    , forecastGridData :: URL
--    , observationStations :: URL
--    , relativeLocation :: Location
--    , forecastZone :: URL
--    , county :: URL
--    , fireWeatherZone :: URL
--    , timeZone :: String
--    , radarStation :: String
--    } deriving (Show)

--instance FromJSON WeatherProperties where
--    parseJSON (Object v) =
--        WeatherProperties  <$> v .: "@id"
 --                          <*> v .: "@type"
--                           <*> v .: "cwa"
--                           <*> v .: "forecastOffice"
--                           <*> v .: "gridX"
--                           <*> v .: "gridY"
--                           <*> v .: "forecast"
--                           <*> v .: "forecastHourly"
--                           <*> v .: "forecastGridData"
--                           <*> v .: "observationStations"
--                           <*> v .: "relativeLocation"
--                           <*> v .: "forecastZone"
--                           <*> v .: "county"
--                           <*> v .: "fireWeatherZone"
--                          <*> v .: "timeZone"
--                           <*> v .: "radarStation"

data WeatherResponse  = WeatherResponse {
      responseContext :: Object
    , responseId :: URL
    , responseJsonType :: String
    , geometry :: String
    , cwa :: String
    , forecastOffice :: URL
    , gridX :: Int
    , gridY :: Int
    , forecast :: URL
    , forecastHourly :: URL
    , forecastGridData :: URL
    , observationStations :: URL
    , relativeLocation :: Location
    , forecastZone :: URL
    , county :: URL
    , fireWeatherZone :: URL
    , timeZone :: String
    , radarStation :: String
    } deriving (Show)


instance FromJSON WeatherResponse where
    parseJSON (Object v) =
        WeatherResponse <$> v .: "@context"
                        <*> v .: "@id"
                        <*> v .: "@type"
                        <*> v .: "geometry"
                        <*> v .: "cwa"
                        <*> v .: "forecastOffice"
                        <*> v .: "gridX"
                        <*> v .: "gridY"
                        <*> v .: "forecast"
                        <*> v .: "forecastHourly"
                        <*> v .: "forecastGridData"
                        <*> v .: "observationStations"
                        <*> v .: "relativeLocation"
                        <*> v .: "forecastZone"
                        <*> v .: "county"
                        <*> v .: "fireWeatherZone"
                        <*> v .: "timeZone"
                        <*> v .: "radarStation"


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
printResults (Right results) = print results


main :: IO ()
main = do
    print "Wouldn't you like to know, weather boi"
    response <- httpLBS request
    let (Status code message) = getResponseStatus response
    if code == 200
        then do
            let jsonBody = getResponseBody response
            L.writeFile "data.json" jsonBody
            jsonData <- L.readFile "data.json" -- could just use jsonBody, but want to document readFile usage
            let weatherResponse = eitherDecode jsonData :: Either String WeatherResponse
            printResults weatherResponse 
            print "saved response to data.json"
        else do
            print "failed request"
            print $ "error code: " ++ show code
            print $ "error message: " ++ show message

