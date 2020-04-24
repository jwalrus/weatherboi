module Main where

import qualified Data.Text as T
import           Points as P
import qualified Forecast as F
import qualified GeoNamesLatLong as LL


main :: IO ()
main = do
    -- todo: get address, state, zip from user and use to construct lat/lng request
    latLng <- LL.fetchLatLong "US+Glen+Elder+State+Park+KS"
    case latLng of
        Nothing -> print "Failed to get latitude / longitude for request"
        Just (LL.LatLong lat lng) -> do
            weatherPoints <- P.fetchWeatherPoints lat lng
            case weatherPoints of
                Nothing -> print "Failed to get points data from national weather service"
                Just (weatherResponse) -> do
                    F.fetchForecast weatherResponse

