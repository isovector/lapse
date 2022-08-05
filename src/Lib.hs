{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE RecordWildCards #-}

module Lib where

import Control.Concurrent.Async
import Data.Time (formatTime, defaultTimeLocale, fromGregorian, parseTimeM, addDays)
import System.IO.Temp (withSystemTempDirectory)
import System.Process
import Diagrams.Prelude
import Diagrams.Backend.Rasterific (B, renderRasterific)
import Diagrams.Backend.Rasterific.Text (texterific)
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Foldable (asum)
import Data.Bool (bool)
import Data.Maybe (mapMaybe)
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


data GitStats = GitStats
  { gs_files :: Int
  , gs_adds :: Int
  , gs_dels :: Int
  , gs_commits :: Int
  }
  deriving (Eq, Ord, Show)

getGitStats :: String -> IO GitStats
getGitStats sha = do
  res <- readProcess "git" ["diff", "--shortstat", sha, "HEAD"] ""
  comms <- readProcess "git" ["rev-list", "--count", sha <> "..HEAD"] ""
  pure $
    case words res of
      [] -> GitStats 0 0 0 0
      [files, _, _, adds, _, dels, _] ->
        GitStats (read files) (read adds) (read dels) (read comms)
      x -> error $ "bad format! " <> show x


getNow :: IO LocalTime
getNow = do
  tz <- getCurrentTimeZone
  utcToLocalTime tz <$> getCurrentTime

clockPanel :: LocalTime -> Diagram B
clockPanel now = vcat' (def & sep .~ 20) $ fmap (centerX . heading)
  [ formatTime defaultTimeLocale "%b%e, %Y" now
  , formatTime defaultTimeLocale "%T" now
  ]


printDiff :: NominalDiffTime -> Diagram B
printDiff now = vcat
  [ stat $ formatTime defaultTimeLocale "%d days %H hours %M minutes" now
  ]


data SelfStats = SelfStats
  { ss_keystrokes :: Int
  , ss_keyseqs :: Int
  , ss_clicks :: Int
  , ss_mouses :: Int
  , ss_total :: DiffTime
  , ss_sessions :: [Session]
  }
  deriving (Eq, Ord, Show)

data Session = Session
  { s_start :: LocalTime
  , s_end :: LocalTime
  , s_dur   :: NominalDiffTime
  }
  deriving (Eq, Ord, Show)

getSelfStats :: IO SelfStats
getSelfStats = do
  (stats : _ : total_str : _ : _ : periodsl) <-
    fmap lines $ readProcess "selfstats" ["-p", "", "-b", "1", "w", "--periods", "180"] ""
  let total_time_str = drop 2 $ dropWhile (/= ':') total_str
  total_time <-
    asum $ fmap (flip (parseTimeM True defaultTimeLocale) total_time_str)
      [ "%dd %Hh %Mm %Ss"
      , "%hh %Mm %Ss"
      , "%Mm %Ss"
      ]
  let Just sessions = traverse parseSession $ filter (not . null) periodsl
  pure $
    case words $ stats of
      [kstr,_,_,kseq,_,_,clicks,_,_,_,_,mouse,_,_] ->
        SelfStats (read kstr) (read kseq) (read clicks) (read mouse) total_time sessions
      _ -> SelfStats 0 0 0 0 total_time sessions

parseSession :: String -> Maybe Session
parseSession s = do
  let date_str = take 10 s
      start_str = take 8 $ drop 11 s
      end_str = take 8 $ drop 22 s
  date <- parseTimeM True defaultTimeLocale "%Y-%m-%d" date_str
  start_time <- parseTimeM True defaultTimeLocale "%T" start_str
  end_time <- parseTimeM True defaultTimeLocale "%T" end_str
  let start = LocalTime date start_time
      end = LocalTime (bool (addDays 1) id (start_time < end_time) date) end_time
  pure $ Session start end $ diffLocalTime end start



main :: IO ()
main = do
    let start = LocalTime (fromGregorian 2022 8 4) $ TimeOfDay 21 0 0
  -- for_ [0..10] $ const $ do
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

spyPanel :: SelfStats -> Diagram B
spyPanel ss =
  vcat' (def & sep .~ 40 & catMethod .~ Distrib)
    [ stat $ show (ss_keystrokes ss) <> " keystrokes"
    , stat $ show (ss_keyseqs ss) <> " key sequences"
    , stat $ show (ss_clicks ss) <> " clicks"
    , stat $ show (ss_mouses ss) <> " mouse moves"
    ]

gitPanel :: GitStats -> Diagram B
gitPanel gs =
  vcat' (def & sep .~ 40 & catMethod .~ Distrib)
    [ stat $ show (gs_commits gs) <> " commits"
    , stat $ show (gs_files gs) <> " files changed"
    , stat $ show (gs_adds gs) <> " additions"
    , stat $ show (gs_dels gs) <> " deletions"
    ]

workPanel :: LocalTime -> SelfStats -> Diagram B
workPanel now ss =
  vcat' (def & sep .~ 50 & catMethod .~ Distrib)
    [ midsize $ formatSession (s_dur $ last $ ss_sessions ss) <> "  this session"
    , midsize $ formatSession (sumNDT $ mapMaybe (todayPortion now) $ ss_sessions ss) <> "  today"
    , midsize $ show (ss_total ss) <> "  this week"
    ]

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


sumNDT :: [NominalDiffTime] -> NominalDiffTime
sumNDT = secondsToNominalDiffTime . sum . fmap nominalDiffTimeToSeconds

formatSession :: NominalDiffTime -> String
formatSession =
  formatTime defaultTimeLocale "%hh %Mm"



tags :: [String]
tags =
  [ "prj:go-go"
  ]


getScreenshot :: FilePath -> IO (Diagram B)
getScreenshot dir = do
  let fp = dir <> "/screenshot.png"
  callProcess "scrot" ["-p", "-f", fp]
  Right png <- loadImageEmb fp
  pure $ image png

getWebcam :: FilePath -> IO (Diagram B)
getWebcam dir = do
  let fp = dir <> "/webcam%03d.png"
  callProcess "ffmpeg"
    [ "-f", "video4linux2"
    , "-i", "/dev/v4l/by-id/usb-Azurewave_Integrated_Camera-video-index0"
    , "-vframes", "1"
    , "-video_size", "640x480"
    , fp
    ]
  Right png <- loadImageEmb $ dir <> "/webcam001.png"
  pure $ scale 0.5 $ image png



