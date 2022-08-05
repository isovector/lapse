module GitPanel where

import System.Process
import Diagrams.Prelude
import Diagrams.Backend.Rasterific (B)
import Types
import Utils

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

gitPanel :: GitStats -> Diagram B
gitPanel gs =
  vcat' (def & sep .~ 40 & catMethod .~ Distrib)
    [ stat $ show (gs_commits gs) <> " commits"
    , stat $ show (gs_files gs) <> " files changed"
    , stat $ show (gs_adds gs) <> " additions"
    , stat $ show (gs_dels gs) <> " deletions"
    ]

