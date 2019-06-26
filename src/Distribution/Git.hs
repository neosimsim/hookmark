module Distribution.Git
  ( compileAbbrHash
  ) where

import           Language.Haskell.TH
import           System.Process

compileAbbrHash :: Q Exp
compileAbbrHash =
  runIO (readProcess "git" ["log", "-1", "--pretty=format:%h"] []) >>= stringE
