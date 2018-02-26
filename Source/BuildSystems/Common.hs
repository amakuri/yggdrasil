
module BuildSystems.Common
  (
  )
where

import Path

data BuildSystem = BuildSystem
  {
    build :: Path Rel File -> IO ()
  }

