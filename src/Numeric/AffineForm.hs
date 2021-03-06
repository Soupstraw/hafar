module Numeric.AffineForm (AFM, AF, newEps,
                           newFromInterval,
                           singleton,
                           evalAFM,
                           radius,
                           midpoint,
                           inf, sup,
                           interval,
                           member,
                           epscount_,
                           setMidpoint,
                           fix,
                           addError,
                           (.+), (.*)
                          ) where

import Numeric.AffineForm.Internal
