{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Lib where

import Control.Concurrent.Async
import Data.Time (getCurrentTime, utcToLocalTime, formatTime, defaultTimeLocale, getCurrentTimeZone, fromGregorian)
import System.IO.Temp (withSystemTempDirectory)
import System.Process
import Diagrams.Prelude
import Diagrams.Backend.Rasterific (B, renderRasterific)
import Diagrams.Backend.Rasterific.Text (texterific)
import Data.Time.LocalTime
import Data.Time.Clock


mkText :: Double -> String -> Diagram B
mkText sz
  = scale (10 * sz) . texterific


heading :: String -> Diagram B
heading = mkText 3


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

printTime :: LocalTime -> Diagram B
printTime now = vcat
  [ heading $ formatTime defaultTimeLocale "%b%e, %Y" now
  , heading $ formatTime defaultTimeLocale "%T" now
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
  }
  deriving (Eq, Ord, Show)

getSelfStats :: IO SelfStats
getSelfStats = do
  (stats : _ : total : _ : _ : periods) <- fmap lines $ readProcess "selfstats" ["-p", "", "-b", "1", "w", "--periods", "180"] ""
  print (total, periods)
  pure $
    case words $ stats of
      [kstr,_,_,kseq,_,_,clicks,_,_,_,_,mouse,_,_] ->
        SelfStats (read kstr) (read kseq) (read clicks) (read mouse)
      _ -> SelfStats 0 0 0 0


main :: IO ()
main = do
    let start = LocalTime (fromGregorian 2022 8 4) $ TimeOfDay 21 0 0
  -- for_ [0..10] $ const $ do
    withSystemTempDirectory "lapse" $ \dir -> do
      (screen, wc, now, gs, ss) <- runConcurrently $
        (,,,,)
          <$> Concurrently (getScreenshot dir)
          <*> Concurrently (getWebcam dir)
          <*> Concurrently getNow
          <*> Concurrently (getGitStats "a1d41c8b00057d415ac96369f557f67fa4134846")
          <*> Concurrently getSelfStats
      renderRasterific "/tmp/out.png" (dims $ V2 640 480) $  vcat
        [ screen
        , centerX $ hcat' (def & sep .~ 40)
            [ centerY wc
            , centerY $ printTime now
            , centerY $ rect 1 240 # fc black
            , centerY $ vcat' (def & sep .~ 50 & catMethod .~ Distrib)
                [ stat $ show (ss_keystrokes ss) <> " keystrokes"
                , stat $ show (ss_keyseqs ss) <> " key sequences"
                , stat $ show (ss_clicks ss) <> " clicks"
                , stat $ show (ss_mouses ss) <> " mouse moves"
                ]
            , centerY $ rect 1 240 # fc black
            , centerY $ vcat' (def & sep .~ 50 & catMethod .~ Distrib)
                [ stat $ show (gs_commits gs) <> " commits"
                , stat $ show (gs_files gs) <> " files changed"
                , stat $ show (gs_adds gs) <> " additions"
                , stat $ show (gs_dels gs) <> " deletions"
                ]
            ]
        ]

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



