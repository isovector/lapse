{-# OPTIONS_GHC -Wall #-}

module ClockPanel where

import Data.Time (formatTime, defaultTimeLocale)
import Diagrams.Prelude
import Diagrams.Backend.Rasterific (B)
import Data.Time.LocalTime
import Utils


clockPanel :: LocalTime -> Diagram B
clockPanel now = vcat' (def & sep .~ 20) $ fmap (centerX . heading)
  [ formatTime defaultTimeLocale "%b%e, %Y" now
  , formatTime defaultTimeLocale "%T" now
  ]

