module Hopinion.Check.Package.ValidateExtendsGeneric.Fact
  ( UnshapedValidate (..),
    ValidateSite (..),
    ValidateShape (..),
  )
where

import Data.Text (Text)
import Hopinion.Facts.Instance
import Hopinion.Facts.Place

-- | A @validate@ written out in an instance of a class named Validity, that is
-- not the structural check followed by the type's own checks.
--
-- Of that name, because the source alone cannot say which class it is: that
-- takes the module the class is defined in, which only the compiler knows.
--
-- The span covers what gets rewritten rather than the instance: the method, or
-- the top-level binding the method names.
data UnshapedValidate = UnshapedValidate
  { unshapedValidateInstance :: !InstanceFact,
    unshapedValidateSite :: !ValidateSite,
    unshapedValidateShape :: !ValidateShape,
    unshapedValidateSpan :: !Span
  }

-- | Where the checks of a @validate@ are written.
--
-- A method may be nothing but the name of a top-level binding of its module,
-- because a binding can carry annotations, such as which mutations to leave
-- out, that GHC does not let an instance method carry.
-- Then that binding is what has to have the shape.
data ValidateSite
  = ValidateSiteMethod
  | -- | The top-level binding of this name in the instance's module.
    ValidateSiteBinding !Text

-- | What a @validate@ of the wrong shape is instead.
--
-- Told apart because the fix differs: a restated default is deleted, and any
-- other shape is rewritten.
data ValidateShape
  = -- | @genericValidate@ of the argument and nothing else, which is what the
    -- method does when it is left out.
    ValidateShapeRestatesDefault
  | -- | @valid@ whatever the argument is, which claims a check while making
    -- none.
    ValidateShapeAlwaysValid
  | -- | Nothing but the name of a binding of this module that is not next to
    -- the instance, where a reader of the instance would not see it.
    ValidateShapeBindingApart
  | ValidateShapeOther
