module External where

import Foreign.C.Types

pureFunc :: CDouble -> CDouble
pureFunc x = x*x - 2
