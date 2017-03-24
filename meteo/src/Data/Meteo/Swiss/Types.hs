{-# LANGUAGE OverloadedStrings #-}

-- | Data types for the Swiss Meteo Net API
module Data.Meteo.Swiss.Types
 (
 -- * Types
   SmnRecord (..)
 , SmnStation(..)
 ) where

import           Data.Aeson
import qualified Data.Text as T
import           Data.Time
import           Data.Default
import           Data.Aeson.Types (Parser) 
import           Data.String (IsString)
import           Control.Monad (mzero)

-- | A Swiss Meteo Network data SmnRecord.
-- It contains the meteo information of a 10 min interval of a Swiss meteo station.
-- 
-- /Note:/ All 'Text' fields may contain empty strings.
data SmnRecord = SmnRecord 
  { smnStation       :: Maybe SmnStation   -- ^  Info about the station
  , smnCode          :: T.Text             -- ^ 3-char all upper-case station code,
  , smnDateTime      :: Maybe UTCTime      -- ^ Time in UTC,
  , smnTemperature   :: T.Text             -- ^ Air temperature 2 m above ground; current value,
  , smnSunshine      :: T.Text             -- ^ Sunshine duration; ten minutes total,
  , smnPrecipitation :: T.Text             -- ^ Precipitation; ten minutes total,
  , smnWindDirection :: T.Text             -- ^ Wind direction; ten minutes mean,
  , smnWindSpeed     :: T.Text             -- ^ Wind speed; ten minutes mean,
  , smnQnhPressure   :: T.Text             -- ^ Pressure reduced to sea level according to standard atmosphere (QNH); current value,
  , smnGustPeak      :: T.Text             -- ^ Gust peak (one second); maximum,
  , smnHumidity      :: T.Text             -- ^ Relative air humidity 2 m above ground; current value,
  , smnQfePressure   :: T.Text             -- ^ Pressure at station level (QFE); current value,
  , smnQffPressure   :: T.Text             -- ^ Pressure reduced to sea level (QFF); current value
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

instance Default SmnRecord where
  def = SmnRecord 
    { smnStation = def
    , smnCode = "" 
    , smnDateTime = Nothing
    , smnTemperature = ""
    , smnSunshine = ""
    , smnPrecipitation = ""
    , smnWindDirection = ""
    , smnWindSpeed = ""
    , smnQnhPressure = ""
    , smnGustPeak = ""
    , smnHumidity = ""
    , smnQfePressure = ""
    , smnQffPressure = ""
  }

-- | A Swiss Meteo Network data station.
--
-- /Note:/ All 'Text' fields may contain empty strings.
data SmnStation = SmnStation 
  { staCode          :: T.Text         -- ^ 3-char all upper-case station code,
  , staName          :: T.Text         -- ^  Original name in local language,
  , staCh1903Y       :: Maybe Int      -- ^  CH1903 (Swiss grid) y-axis value,
  , staCh1903X       :: Maybe Int      -- ^  CH1903 (Swiss grid) x-axis value,
  , staLat           :: Maybe Double   -- ^  WGS84 latitude,
  , staLng           :: Maybe Double   -- ^  WGS84 longitude,
  , staElevation     :: Maybe Int      -- ^  meters above sea level
  }
 deriving Show

instance FromJSON SmnStation where 
  parseJSON (Object v) = SmnStation
    <$> v .:?! "code"
    <*> v .:?! "name"
    <*> v .:? "ch1903Y"
    <*> v .:? "ch1903X"
    <*> v .:? "lat"
    <*> v .:? "lng"
    <*> v .:? "elevation"
  parseJSON _ = mzero

instance Default SmnStation where
  def = SmnStation 
    { staCode = ""
    , staName = ""
    , staCh1903Y = Nothing
    , staCh1903X = Nothing
    , staLat = Nothing
    , staLng = Nothing
    , staElevation = Nothing
  }
 
-- little helper combinator to parse and convert null values to empty Text strings.
(.:?!) :: (Data.String.IsString a, FromJSON a) => Object -> T.Text -> Parser a
(.:?!) v k = v .: k .!= ""