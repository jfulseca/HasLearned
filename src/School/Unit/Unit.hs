module School.Unit.Unit
( Unit(..) ) where

import School.Unit.UnitActivation (UnitActivation(..))
import School.Unit.UnitGradient (UnitGradient(..))
import School.Unit.UnitParams (UnitParams(..))

data Unit a = Unit
  { apply :: UnitParams a
          -> UnitActivation a
          -> UnitActivation a
  , deriv :: UnitParams a
          -> UnitGradient a
          -> UnitActivation a
          -> ( UnitGradient a
             , UnitParams a
             )
  }
    
