module Hopinion.Check.Package.ValidateExtendsGeneric.Fact
  ( UnshapedValidate (..),
    ValidateShape (..),
  )
where

import Hopinion.Facts.Instance
import Hopinion.Facts.Place

-- | A @validate@ written out in an instance of a class named Validity, that is
-- not the structural check followed by the type's own checks.
--
-- Of that name, because the source alone cannot say which class it is: that
-- takes the module the class is defined in, which only the compiler knows.
--
-- The span covers the method rather than the instance, because the method is
-- what gets rewritten.
data UnshapedValidate = UnshapedValidate
  { unshapedValidateInstance :: !InstanceFact,
    unshapedValidateShape :: !ValidateShape,
    unshapedValidateSpan :: !Span
  }

-- | What a @validate@ of the wrong shape is instead.
--
-- Told apart because the fix differs: a restated default is deleted, and any
-- other shape is rewritten.
data ValidateShape
  = -- | @genericValidate@ of the argument and nothing else, which is what the
    -- method does when it is left out.
    ValidateShapeRestatesDefault
  | ValidateShapeOther
