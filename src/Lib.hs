{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Lib where

import Control.Concurrent.Async
import Data.Time (getCurrentTime, utcToLocalTime, formatTime, defaultTimeLocale, getCurrentTimeZone)
import Graphics.Gloss
import Graphics.Gloss.Export.PNG
import Graphics.Gloss.Juicy
import Graphics.Htdp
import Graphics.Htdp.Data.Image (shapes, Image (Image))
import System.IO.Temp (withSystemTempDirectory)
import System.Process


mkText :: Float -> String -> Image
mkText sz
  = toImage 20 (sz * 20 - 10)
  . translate 0 (-10 * sz)
  . scale (0.1 * sz) (0.1 * sz)
  . color (makeColor 0 0 0 1)
  . text


heading :: String -> Image
heading = mkText 3


stat :: String -> Image
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


getNow :: IO Image
getNow = do
  tz <- getCurrentTimeZone
  now <- utcToLocalTime tz <$> getCurrentTime
  pure $ aboves
    [ heading $ formatTime defaultTimeLocale "%b%e, %Y" now
    , heading $ formatTime defaultTimeLocale "%T" now
    ]

strut :: Float -> Image
strut h = rectangle 2 h solid $ makeColor 0 0 0 1

main :: IO ()
main = do
  -- for_ [0..10] $ const $ do
    withSystemTempDirectory "lapse" $ \dir -> do
      (screen, wc, now, gs) <- runConcurrently $
        (,,,)
          <$> Concurrently (getScreenshot dir)
          <*> Concurrently (getWebcam dir)
          <*> Concurrently getNow
          <*> Concurrently (getGitStats "a1d41c8b00057d415ac96369f557f67fa4134846")
      let sides = (1920 - 320) / 2
      exportImage  (makeColor 1 0 1 1) "/tmp/out.png" $ aboves
        [ screen
        , besides
            [ rectangle sides 100 solid $ makeColor 1 1 1 1
            , strut 240
            , wc
            , strut 240
            , let txt = now
                  bg = rectangle sides 240 solid $ makeColor 0.9 0.9 0.9 1
              in flip (overlayAlign low high) bg $ aboves
                  [ txt
                  , stat $ show (gs_commits gs) <> " commits"
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

exportImage :: Color -> FilePath -> Image -> IO ()
exportImage c fp img@(Image w h _) =
  exportPictureToPNG (round w, round h) c fp $ toPicture img


getScreenshot :: FilePath -> IO Image
getScreenshot dir = do
  let fp = dir <> "/screenshot.png"
  callProcess "scrot" ["-p", "-f", fp]
  Just png <- loadJuicyPNG fp
  pure $ toImage 1920 1080 png

getWebcam :: FilePath -> IO Image
getWebcam dir = do
  let fp = dir <> "/webcam%03d.png"
  callProcess "ffmpeg"
    [ "-f", "video4linux2"
    , "-i", "/dev/v4l/by-id/usb-Azurewave_Integrated_Camera-video-index0"
    , "-vframes", "1"
    , "-video_size", "640x480"
    , fp
    ]
  Just png <- loadJuicyPNG $ dir <> "/webcam001.png"
  pure $ toImage 320 240 $ scale 0.5 0.5 png



toImage :: Float -> Float -> Picture -> Image
toImage w h = Image w h . pure . (, (0, 0))


toPicture :: Image -> Picture
toPicture
  = Pictures
  . fmap (\(p, (x, y)) -> translate x y p)
  . shapes

