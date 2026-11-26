module Hopinion.Check.Package.NoRestatedGenValidDefaults.Fact (RestatedMethod (..)) where

import Data.Text (Text)
import Hopinion.Facts.Instance
import Hopinion.Facts.Place

-- | One method an instance writes out as exactly what a class of that name
-- would use had the method been left out.
--
-- Of that name, because the source alone cannot say which class it is: that
-- takes the module the class is defined in, which only the compiler knows.
--
-- The span covers the method rather than the instance, because each such
-- method is deleted on its own, and the rest of the instance may well stay.
data RestatedMethod = RestatedMethod
  { restatedMethodInstance :: !InstanceFact,
    restatedMethodName :: !Text,
    restatedMethodSpan :: !Span
  }
