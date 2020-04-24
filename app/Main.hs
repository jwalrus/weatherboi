module Main where

import qualified Data.Text as T
import           Points as P
import qualified Forecast as F
import qualified GeoNamesLatLong as LL


main :: IO ()
main = do
    putStrLn "Enter address or landmark"
    addr <- getLine
    let addrr = (T.intercalate "+" . T.words . T.pack) addr
    putStrLn "Enter state"
    state <- getLine
    let search = T.intercalate "+" ["US", addrr, (T.pack state)]
    -- todo: get address, state, zip from user and use to construct lat/lng request
    latLng <- LL.fetchLatLong search -- "US+Glen+Elder+State+Park+KS"
    case latLng of
        Nothing -> print "Failed to get latitude / longitude for request"
        Just (LL.LatLong lat lng) -> do
            weatherPoints <- P.fetchWeatherPoints lat lng
            case weatherPoints of
                Nothing -> print "Failed to get points data from national weather service"
                Just (weatherResponse) -> do
                    F.fetchForecast weatherResponse

