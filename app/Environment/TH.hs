module Environment.TH where

import Language.Haskell.TH.Quote (quoteFile, QuasiQuoter)
import Text.RawString.QQ (r)

rf :: QuasiQuoter
rf = quoteFile r
