module Types where

import Data.Time (NominalDiffTime, LocalTime)


data GitStats = GitStats
  { gs_files :: Int
  , gs_adds :: Int
  , gs_dels :: Int
  , gs_commits :: Int
  }
  deriving (Eq, Ord, Show)


data SelfStats = SelfStats
  { ss_keystrokes :: Int
  , ss_keyseqs :: Int
  , ss_clicks :: Int
  , ss_mouses :: Int
  , ss_total :: NominalDiffTime
  , ss_sessions :: [Session]
  }
  deriving (Eq, Ord, Show)

instance Semigroup SelfStats where
  SelfStats n i j x ndt sess <> SelfStats y z w int ndt' sess'
    = SelfStats
        { ss_keystrokes = n + y
        , ss_keyseqs = i + z
        , ss_clicks = j + w
        , ss_mouses = x + int
        , ss_total = ndt + ndt'
        , ss_sessions = sess <> sess'
        }


data Session = Session
  { s_start :: LocalTime
  , s_end :: LocalTime
  , s_dur   :: NominalDiffTime
  }
  deriving (Eq, Ord, Show)

