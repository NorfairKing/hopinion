module Hopinion.Check.Project.NoCatchAllOwnSum.Fact
  ( CatchAll (..),
    SumType (..),
    Synonym (..),
    DataInstance (..),
  )
where

import Data.Text (Text)
import Hopinion.Facts.Name
import Hopinion.Facts.Place

-- | One top-level pattern that matches whatever the alternatives before it
-- left over, in a column where another alternative takes the same value apart
-- by constructor.
--
-- Which type that value has is not written in the source, so this is every
-- such pattern, whatever it matches on.
-- The span is the wildcard or the variable alone, because that is the code the
-- compiler gave the value's type to.
data CatchAll = CatchAll
  { catchAllSpan :: !Span,
    -- | The declaration it is written in, if any, which is what a suppression
    -- names.
    catchAllDecl :: !(Maybe DeclName)
  }

-- | One data type a module declares with more than one constructor.
newtype SumType = SumType
  { unSumType :: Text
  }

-- | One type synonym a module declares, with where the name at the head of its
-- right-hand side is written.
--
-- A pattern on a value whose type was written with the synonym has the
-- synonym as its type, so a synonym of a sum type is that sum type too.
data Synonym = Synonym
  { synonymName :: !Text,
    synonymHead :: !Span
  }

-- | One instance of a data family with more than one constructor, by where the
-- family and the head of each argument are written.
--
-- An argument with no name at its head, such as a type variable, is 'Nothing'.
data DataInstance = DataInstance
  { dataInstanceFamily :: !Span,
    dataInstanceArguments :: ![Maybe Span]
  }
