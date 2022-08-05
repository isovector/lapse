{-# OPTIONS_GHC -Wall #-}

module Lib where

import ClockPanel
import Control.Concurrent.Async
import Data
import Diagrams.Backend.Rasterific (renderRasterific)
import Diagrams.Prelude hiding (value, Options)
import GitPanel
import SpyPanels
import System.IO.Temp (withSystemTempDirectory)
import Utils
import Options.Applicative
import System.Process (readProcess)
import System.Directory (getDirectoryContents)
import Control.Monad (forever)


data Options = Options
  { o_sha :: Maybe String
  , o_dir :: FilePath
  }


optsParser :: ParserInfo Options
optsParser =
  info (helper <*> versionOption <*> programOptions) $ mconcat
    [ fullDesc
    , header "lapse - make programming timelapses"
    ]


versionOption :: Parser (a -> a)
versionOption = infoOption "1.0" $ long "version" <> help "Show version"


programOptions :: Parser Options
programOptions =
  Options
    <$> optional (strOption (long "sha" <> metavar "SHA" <> help "SHA to base git stats relative to"))
    <*> argument str (metavar "OUTPUT-DIR" <> help "Output directory")


main :: IO ()
main = do
  opts <- execParser optsParser
  sha <- maybe getSha pure $ o_sha opts

  forever $ do
    withSystemTempDirectory "lapse" $ \dir -> do
      (screen, webcam, now, gs, ss, fp) <- runConcurrently $
        (,,,,,)
          <$> Concurrently (getScreenshot dir)
          <*> Concurrently (getWebcam dir)
          <*> Concurrently getNow
          <*> Concurrently (getGitStats sha)
          <*> Concurrently getSelfStats
          <*> Concurrently (getNextFileName $ o_dir opts)
      let panel_sep = rect 2 240 # fc grey # lw 0
          background = rect 1920 240 # fc (darken 0.05 darkviolet) # lw 0
      renderRasterific fp(dims $ V2 1920 (1080 + 240)) $  vcat
        [ screen
        , flip mappend background $ centerX $ hcat' (def & sep .~ 40) $ fmap centerY
            [ spyPanel ss
            , panel_sep
            , gitPanel gs
            , webcam
            , clockPanel now
            , panel_sep
            , workPanel now ss
            ]
        ]


getNextFileName :: FilePath -> IO FilePath
getNextFileName dir = do
  sz <- fmap length $ getDirectoryContents dir
  pure $ dir <> "/" <> lpad 10 '0' (show sz) <> ".png"


lpad :: Int -> Char -> String -> String
lpad sz c s = replicate (sz - length s) c <> s


getSha :: IO String
getSha
  = fmap (head . filter (not . null) . lines)
  $ readProcess "git" ["rev-parse", "HEAD"] ""

