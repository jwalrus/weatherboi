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
                                    $ setRequestHeader "User-Agent" [ua]
                                    $ setRequestPath path
                                    $ setRequestSecure True
                                    $ setRequestPort 443
                                    $ defaultRequest

request :: Request
request = buildRequest weatherHost "GET" apiPath userAgent


main :: IO ()
main = do
    print "Wouldn't you like to know, weather boi"
    response <- httpLBS request
    let (Status code message) = getResponseStatus response
    if code == 200
        then do
            let jsonBody = getResponseBody response
            L.writeFile "data.json" jsonBody
            print "saved response to data.json"
        else do
            print "failed request"
            print $ "error code: " ++ show code
            print $ "error message: " ++ show message

