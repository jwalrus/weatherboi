module Forecast where

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import           Data.Aeson
import           Data.List (stripPrefix)
import qualified Data.Text as T
import           GHC.Generics
import           Lib
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status
import qualified Points as P


type URL = T.Text 
type Timestamp = T.Text -- todo: find time package

data Forecast = Forecast {
      forecastContext :: Object
    , geometry :: T.Text
    , updated :: Timestamp
    , units :: T.Text
    , forecastGenerator :: T.Text
    , updateTime :: Timestamp
    , validTimes :: Timestamp
    , elevation :: Elevation
    , periods :: [Period]
    } deriving (Show)

instance FromJSON Forecast where
    parseJSON (Object v) =
        Forecast <$> v .: "@context"
                 <*> v .: "geometry"
                 <*> v .: "updated"
                 <*> v .: "units"
                 <*> v .: "forecastGenerator"
                 <*> v .: "updateTime"
                 <*> v .: "validTimes"
                 <*> v .: "elevation"
                 <*> v .: "periods"

data Elevation = Elevation {
      elevationValue :: Double
    , elevationUnitCode :: T.Text
    } deriving (Show, Generic)

instance FromJSON Elevation where
    parseJSON (Object v) =
        Elevation <$> v .: "value"
                  <*> v .: "unitCode"

data Period = Period {
      number :: Int
    , name :: T.Text
    , startTime :: Timestamp
    , endTime :: Timestamp
    , isDaytime :: Bool
    , temperature :: Int
    , temperatureUnit :: Char
    , temperatureTrend :: Maybe T.Text
    , windSpeed :: T.Text
    , windDirection :: T.Text
    , icon :: URL
    , shortForecast :: T.Text
    , detailedForecast :: T.Text
    } deriving (Show, Generic)

instance FromJSON Period where
    parseJSON = genericParseJSON defaultOptions
        { omitNothingFields = True }



getForecastPath :: P.WeatherResponse -> Maybe T.Text
getForecastPath weather = T.stripPrefix (T.append "https://" weatherHost) $ P.forecast weather


fetchForecast :: P.WeatherResponse -> IO ()
fetchForecast results = do
    let forecastPath = getForecastPath results
    case forecastPath of
        Nothing -> print "failed to find forecast"
        Just (path) -> do
            response <- httpLBS $ buildHttpsRequest "GET" weatherHost path
            let (Status code message) = getResponseStatus response
            if code == 200
                then do
                    let jsonBody = getResponseBody response
                    let forecastResponse = eitherDecode jsonBody :: Either String Forecast
                    case forecastResponse of
                      Left (err) -> print $ "Failed to decode forecast: " ++ err
                      Right (f) -> print $ "Forecast: " ++ show f
                    L.writeFile "forecast.json" jsonBody
                    print "saved forecast data"
                else do
                    print "failed forecast request"
                    print $ "error code: " ++ show code
                    print $ "error msg: " ++ show message


