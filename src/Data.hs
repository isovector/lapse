{-# OPTIONS_GHC -Wall #-}

module Data where

import System.Process
import Diagrams.Prelude
import Diagrams.Backend.Rasterific (B)


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


getScreenshot :: FilePath -> IO (Diagram B)
getScreenshot dir = do
  let fp = dir <> "/screenshot.png"
  callProcess "scrot" ["-p", "-f", fp]
  Right png <- loadImageEmb fp
  pure $ image png

