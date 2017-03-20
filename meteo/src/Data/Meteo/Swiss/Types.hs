 {-# LANGUAGE OverloadedStrings #-}
 
module Data.Meteo.Swiss.Types where

import           Data.Aeson
import qualified Data.Text as T
import           Data.Time
import           Data.Aeson.Types (Parser) 
import           Data.String (IsString)
import           Control.Monad (mzero)

import qualified Data.ByteString.Lazy as B

data SmnRecord = SmnRecord 
  { smnStation       :: Maybe Station -- optional  Info about the station
  , smnCode          :: T.Text        -- optional: 3-char all upper-case station code,
  , smnDateTime      :: Maybe UTCTime -- optional: Time in UTC,
  , smnTemperature   :: T.Text        -- optional: Air temperature 2 m above ground; current value,
  , smnSunshine      :: T.Text        -- optional: Sunshine duration; ten minutes total,
  , smnPrecipitation :: T.Text        -- optional: Precipitation; ten minutes total,
  , smnWindDirection :: T.Text        -- optional: Wind direction; ten minutes mean,
  , smnWindSpeed     :: T.Text        -- optional: Wind speed; ten minutes mean,
  , smnQnhPressure   :: T.Text        -- optional: Pressure reduced to sea level according to standard atmosphere (QNH); current value,
  , smnGustPeak      :: T.Text        -- optional: Gust peak (one second); maximum,
  , smnHumidity      :: T.Text        -- optional: Relative air humidity 2 m above ground; current value,
  , smnQfePressure   :: T.Text        -- optional: Pressure at station level (QFE); current value,
  , smnQffPressure   :: T.Text        -- optional: Pressure reduced to sea level (QFF); current value
  }
 deriving Show


instance FromJSON SmnRecord where 
  parseJSON (Object v) = SmnRecord
    <$> v .:? "station"
    <*> v .:?! "code" 
    <*> v .:? "dateTime"
    <*> v .:?! "temperature"
    <*> v .:?! "sunshine"
    <*> v .:?! "precipitation"
    <*> v .:?! "windDirection"
    <*> v .:?! "windSpeed"
    <*> v .:?! "qnhPressure"
    <*> v .:?! "gustPeak"
    <*> v .:?! "humidity"
    <*> v .:?! "qfePressure"
    <*> v .:?! "qffPressure"
  parseJSON _ = mzero


data Station = Station 
  { staCode          :: T.Text        -- optional: 3-char all upper-case station code,
  , staName          :: T.Text        -- optional: Original name in local language,
  , staCh1903Y       :: Maybe Int     -- optional: CH1903 (Swiss grid) y-axis value,
  , staCh1903X       :: Maybe Int     -- optional: CH1903 (Swiss grid) x-axis value,
  , staLat           :: Maybe Double  -- optional: WGS84 latitude,
  , staLng           :: Maybe Double  -- optional: WGS84 longitude,
  , staElevation     :: Maybe Int     -- optional: meters above sea level
  }
 deriving Show

instance FromJSON Station where 
  parseJSON (Object v) = Station
    <$> v .:?! "code"
    <*> v .:?! "name"
    <*> v .:? "ch1903Y"
    <*> v .:? "ch1903X"
    <*> v .:? "lat"
    <*> v .:? "lng"
    <*> v .:? "elevation"
  parseJSON _ = mzero
 
  -- | little helper combinator to parse and convert null values to empty test strings.
(.:?!) :: (Data.String.IsString a, FromJSON a) => Object -> T.Text -> Parser a
(.:?!) v k = v .: k .!= ""

{- 
-- ---------------------------------------------------------------------------------------
jsonFile :: FilePath
-- jsonFile = "bern.json"    -- use datatype  IO (Either String SmnRecord)
jsonFile = "all.json"     -- use datatype  IO (Either String [SmnRecord])

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

main :: IO ()
main = do
 -- Get JSON data and decode it
 d <- (eitherDecode <$> getJSON) :: IO (Either String [SmnRecord])
 -- If d is Left, the JSON was malformed.
 -- In that case, we report the error.
 -- Otherwise, we perform the operation of
 -- our choice. In this case, just print it.
 case d of
   Left err -> putStrLn err
   Right ps -> print ps
   -- Right ps -> print (map (getStationName . smnStation) ps)

getStationName :: Maybe Station -> T.Text
getStationName (Just starec) = staName starec
getStationName Nothing = "STATION IS NOTHING"

-}