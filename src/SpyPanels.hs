module SpyPanels where

import Data.Time (defaultTimeLocale, parseTimeM, addDays)
import System.Process
import Diagrams.Prelude
import Diagrams.Backend.Rasterific (B)
import Data.Time.LocalTime
import Data.Foldable (asum)
import Data.Bool (bool)
import Data.Maybe (mapMaybe)
import Types
import Utils


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
      , "%Ss"
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


spyPanel :: SelfStats -> Diagram B
spyPanel ss =
  vcat' (def & sep .~ 40 & catMethod .~ Distrib)
    [ stat $ show (ss_keystrokes ss) <> " keystrokes"
    , stat $ show (ss_keyseqs ss) <> " key sequences"
    , stat $ show (ss_clicks ss) <> " clicks"
    , stat $ show (ss_mouses ss) <> " mouse moves"
    ]


workPanel :: LocalTime -> SelfStats -> Diagram B
workPanel now ss =
  vcat' (def & sep .~ 50 & catMethod .~ Distrib)
    [ midsize $ formatSession (s_dur $ last $ ss_sessions ss) <> "  this session"
    , midsize $ formatSession (sumNDT $ mapMaybe (todayPortion now) $ ss_sessions ss) <> "  today"
    , midsize $ formatSession (ss_total ss) <> "  this week"
    ]

