module GeoNamesLatLong where

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


geoNamesHost :: BC.ByteString
geoNamesHost = "api.geonames.org"

latLongApi :: BC.ByteString
latLongApi = "/geoCodeAddressJSON?q=US+Glen+Elder+State+Park+KS&username=weatherboi"

data Address = Address { address :: AddressDetails } deriving (Show, Generic)
instance FromJSON Address

-- todo: improve parser to correct parse "12345" as Int and "23.9324" as Double
data AddressDetails = AddressDetails {
      adminCode2 :: Maybe T.Text
    , adminCode3 :: Maybe T.Text
    , adminCode1 :: Maybe T.Text
    , lng :: T.Text
    , houseNumber :: Maybe T.Text
    , locality :: Maybe T.Text
    , adminCode4 :: Maybe T.Text
    , adminName2 :: Maybe T.Text
    , street :: Maybe T.Text
    , postalCode :: Maybe Int
    , countryCode :: Maybe T.Text
    , adminName1 :: Maybe T.Text
    , lat :: T.Text
    } deriving (Show, Generic)

instance FromJSON AddressDetails where
    parseJSON = genericParseJSON defaultOptions
        { omitNothingFields = True }

data LatLong = LatLong {
      latitude :: T.Text
    , longitude :: T.Text
    } deriving (Show, Eq)

latLongRequest :: Request
latLongRequest = setRequestMethod "GET" $ setRequestHost geoNamesHost
                                        $ setRequestPath latLongApi 
                                        $ defaultRequest



fetchLatLong :: Request -> IO (Maybe LatLong)
fetchLatLong request = do
    response <- httpLBS request
    let (Status code message) = getResponseStatus response
    if code == 200
        then do
            let jsonBody = getResponseBody response
            let latLongResponse = eitherDecode jsonBody :: Either String Address
            L.writeFile "latlng.json" jsonBody
            case latLongResponse of
              Left (err) -> do
                  print $ "Failed to get lat long because of following error: " ++ err
                  return Nothing
              Right (addr) -> do
                  print addr -- todo: remove after stable impl
                  let details = address addr
                  return $ Just $ LatLong (lat details) (lng details)
        else do
            print "failed request"
            print $ "error code: " ++ show code
            print $ "error message: " ++ show message
            return Nothing
