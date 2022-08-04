{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Lib where

import Control.Concurrent.Async
import Data.Time (getCurrentTime, utcToLocalTime, formatTime, defaultTimeLocale)
import Graphics.Gloss
import Graphics.Gloss.Export.PNG
import Graphics.Gloss.Juicy
import Graphics.Htdp
import Graphics.Htdp.Data.Image (shapes, Image (Image))
import System.IO.Temp (withSystemTempDirectory)
import System.Process


heading :: String -> Image
heading
  = toImage 20 50
  . translate 0 (-30)
  . scale 0.3 0.3
  . color (makeColor 0 0 0 1)
  . text

getGitStats :: String -> IO (Int, Int, Int)
getGitStats sha = do
  res <- readProcess "git" ["diff", "--shortstat", sha, "HEAD"] ""
  pure $
    case words res of
      [] -> (0, 0, 0)
      [files, _, _, adds, _, dels, _] -> (read files, read adds, read dels)
      x -> error $ "bad format! " <> show x




getNow :: IO Image
getNow = do
  now <- utcToLocalTime (read "PST") <$> getCurrentTime
  pure $ aboves
    [ heading $ formatTime defaultTimeLocale "%b%e, %Y" now
    , heading $ formatTime defaultTimeLocale "%T" now
    ]

strut :: Float -> Image
strut h = rectangle 2 h solid $ makeColor 0 0 0 1

main :: IO ()
main = do
    print =<< getGitStats "HEAD~1"
  -- for_ [0..10] $ const $ do
    withSystemTempDirectory "lapse" $ \dir -> do
      (screen, wc, now) <- runConcurrently $
        (,,)
          <$> Concurrently (getScreenshot dir)
          <*> Concurrently (getWebcam dir)
          <*> Concurrently getNow
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
              in overlayAlign low high txt bg
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

