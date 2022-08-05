module Lib where

import ClockPanel
import Control.Concurrent.Async
import Data
import Data.Time (fromGregorian)
import Data.Time.LocalTime
import Diagrams.Backend.Rasterific (renderRasterific)
import Diagrams.Prelude
import GitPanel
import SpyPanels
import System.IO.Temp (withSystemTempDirectory)
import Utils


main :: IO ()
main = do
    let start = LocalTime (fromGregorian 2022 8 4) $ TimeOfDay 21 0 0
    withSystemTempDirectory "lapse" $ \dir -> do
      (screen, webcam, now, gs, ss) <- runConcurrently $
        (,,,,)
          <$> Concurrently (getScreenshot dir)
          <*> Concurrently (getWebcam dir)
          <*> Concurrently getNow
          <*> Concurrently (getGitStats "a1d41c8b00057d415ac96369f557f67fa4134846")
          <*> Concurrently getSelfStats
      let panel_sep = rect 2 240 # fc grey # lw 0
          bg = rect 1920 240 # fc (darken 0.05 darkviolet) # lw 0
      renderRasterific "/tmp/out.png" (dims $ V2 1920 (1080 + 240)) $  vcat
        [ screen
        , flip mappend bg $ centerX $ hcat' (def & sep .~ 40) $ fmap centerY
            [ spyPanel ss
            , panel_sep
            , gitPanel gs
            , webcam
            , clockPanel now
            , panel_sep
            , workPanel now ss
            ]
        ]

