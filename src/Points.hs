module Points where

import           Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Text as T
import           GHC.Generics

type URL = T.Text 

data Geometry  = Geometry {
      geometryJsonType :: T.Text
    , coordinates :: (Double, Double)
    } deriving (Show)

instance FromJSON Geometry where
    parseJSON (Object v) =
        Geometry <$> v .: "type"
                 <*> v .: "coordinates"

data Distance = Distance {
      distanceValue :: Double
    , distanceUnitCode :: T.Text
    } deriving (Show)

instance FromJSON Distance where
    parseJSON (Object v) =
        Distance <$> v .: "value"
                 <*> v .: "unitCode"

data Bearing = Bearing {
      bearingValue :: Int
    , bearingUnitCode :: T.Text
    } deriving (Show)

instance FromJSON Bearing where
    parseJSON (Object v) =
        Bearing <$> v .: "value"
                <*> v .: "unitCode"

data Location = Location {
      city :: T.Text
    , state :: T.Text
    , distance :: Distance
    , bearing :: Bearing
    } deriving (Show, Generic)

instance FromJSON Location

data WeatherResponse  = WeatherResponse {
      responseContext :: Object
    , responseId :: URL
    , responseJsonType :: T.Text
    , geometry :: T.Text
    , cwa :: T.Text
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
    , timeZone :: T.Text
    , radarStation :: T.Text
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


