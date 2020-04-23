module Forecast where

import           Data.Aeson
import qualified Data.Text as T
import           GHC.Generics

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
