module Program where

import qualified P
import qualified Q
import qualified R

f = P.foo
g = Q.bar
h = R.foo

h2 = P.foo . R.foo
