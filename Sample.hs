

import Data.Proposition
import Data.Proposition.Char
import Data.Char


prop :: Prop p => p Char
prop = propAnds [propLit 'a' `propOr` propNot (propLit 'b')
                ,propLit 'c' `propOr` propLit 'c'
                ,propLit 'd' `propOr` propTrue]


upperCase x = propMap (propLit . toUpper) x
