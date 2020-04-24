module GeoNamesLatLong where

import           Data.Aeson
import qualified Data.ByteString.Lazy as L
import           Data.List (stripPrefix)
import qualified Data.Text as T
import           GHC.Generics
import           Lib
import           Network.HTTP.Simple
import           Network.HTTP.Types.Status


geoNamesHost :: T.Text
geoNamesHost = "api.geonames.org"

latLongApi :: T.Text -> T.Text
latLongApi search = mconcat ["/geoCodeAddressJSON?q=", search, "&username=weatherboi"]

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


fetchLatLong :: T.Text -> IO (Maybe LatLong)
fetchLatLong search = do
    response <- httpLBS $ buildHttpRequest "GET" geoNamesHost (latLongApi search)
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
