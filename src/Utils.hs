{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RecordWildCards #-}

module Utils where

import Data.Time (formatTime, defaultTimeLocale)
import Data.Time.Clock
import Data.Time.LocalTime
import Diagrams.Backend.Rasterific (B)
import Diagrams.Backend.Rasterific.Text (texterific)
import Diagrams.Prelude
import Types
import Data.Function (on)


mkText :: Double -> String -> Diagram B
mkText sz
  = fc white . scale (10 * sz) . texterific


heading :: String -> Diagram B
heading = mkText 3

midsize :: String -> Diagram B
midsize = mkText 2.5


stat :: String -> Diagram B
stat = mkText 2


getNow :: IO LocalTime
getNow = do
  tz <- getCurrentTimeZone
  utcToLocalTime tz <$> getCurrentTime


sumNDT :: [NominalDiffTime] -> NominalDiffTime
sumNDT = secondsToNominalDiffTime . sum . fmap nominalDiffTimeToSeconds


formatSession :: NominalDiffTime -> String
formatSession =
  formatTime defaultTimeLocale "%hh %Mm"


todayPortion :: LocalTime -> Session -> Maybe NominalDiffTime
todayPortion now Session{..} =
    case (start_ok, end_ok) of
      (True, True) -> Just s_dur
      (False, True) -> Just $
        -- time from the beginning of the day
        diffLocalTime s_end $ s_end { localTimeOfDay = TimeOfDay 0 0 0 }
      (True, False) -> Just $
        -- time to the end of the day
        diffLocalTime (s_start { localTimeOfDay = TimeOfDay 23 59 0 }) s_start
      (False, False) -> Nothing
  where
    same_day = (==) `on` localDay
    start_ok = same_day now s_start
    end_ok = same_day now s_end

