-- | A Haskell implementation of the Swiss Meteo Net data API
--
-- OpenData SMN is a REST API for <http://www.meteoswiss.admin.ch/home/measurement-and-forecasting-systems/land-based-stations/automatisches-messnetz.html SwissMetNet > data.
-- It's developed in the open supporting the <http://opendata.ch/ OpenData.ch > initiative. 
-- Free OpenData hosting is provided by <http://netcetera.com/ Netcetera > at <http://data.netcetera.com/smn >.
--
-- The actual data for 10 min intervals is provided by opendata.swiss, the Swiss open government data portal.

module Data.Meteo.Swiss 
  (
    module Data.Meteo.Swiss.Types
  , module Data.Meteo.Swiss.Urls
  ) 
  where
import Data.Meteo.Swiss.Types
import Data.Meteo.Swiss.Urls