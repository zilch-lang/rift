module Rift.Environment.TH where

import Language.Haskell.TH.Quote (quoteFile, QuasiQuoter)
import Text.RawString.QQ (r)

-- | A simple quasiquoter allowing reading a raw string from a file path.
rf :: QuasiQuoter
rf = quoteFile r
