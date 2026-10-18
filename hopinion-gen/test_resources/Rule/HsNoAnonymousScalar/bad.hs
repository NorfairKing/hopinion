module Bad where

import qualified Data.ByteString as SB
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import qualified Data.Text as T

newtype RuleName = RuleName Text

newtype OwnerName = OwnerName Text

data SourceRoot = SourceRoot !OwnerName

data ComponentKind = ComponentKindLib

data Verbosity = VerbosityQuiet

-- Two anonymous Texts, and a constructor name can be the name of at most one of
-- them. Which one is the rule and which one is the reason is written nowhere,
-- and a caller that has them the other way round gets no error.
data OIDCRuleError
  = OIDCRuleUnbounded !Text !String
  | OIDCRuleUserUnusable !Text !String

-- Anonymity is a property of one field rather than a relation between two, so
-- fields do not have to share a type to be reported. These three are a name, a
-- length and a limit, and nothing here says in which order.
data PostError = PostErrorTooLong !Text !Int !Int

-- A Maybe holds at most one of the thing, so it is as anonymous as the thing
-- and the fix goes inside it.
data Lookup = LookupMissed !(Maybe Text) !Text

-- A collection is not the thing it collects, so neither of these lists is read
-- as a primitive, and what is reported here is the bare Text beside them. The
-- fix the rule asks for is a type on that one: what the lists want is a
-- question it leaves alone.
data EvalLine = EvalLineErrored !(NonEmpty Text) !Text

data Request = RequestKeyValue ![Text] !Text

-- The names a type is written under say nothing about what a field holds, so
-- this is read after the qualifiers come off.
data Shape = ShapeApplied !T.Text !SB.ByteString

-- [Char] is the one list the rule reads, because it is String spelled out and
-- a primitive read under one spelling and not the other would be a way around
-- the rule that nothing else closes.
data Decode = DecodeFailed ![Char] !String

-- A product written positionally is a constructor like any other. Here the
-- cheaper fix is record syntax rather than two newtypes, because a start and an
-- end want names more than they want types.
data Segment = Segment !Int !Int

-- Strictness is not what the rule reads.
data Window = Window Int Int

-- A constructor of one field is named by that name, and a constructor of
-- several is not, so the reason the first of these is silent is that it has
-- nothing a name has to pick out and the others do.
data Answer
  = AnswerUnusable !Text
  | AnswerRefused !RuleName !Text

-- One primitive among fields that are types is still named nowhere: the
-- constructor names what it holds altogether, and a reader looking for which
-- of the four holds the owner counts to find out.
data Dispatch = DispatchProject !SourceRoot !Text !ComponentKind !Verbosity

-- Fields are confusable with the ones beside them and no others, so this is
-- two findings and not three: the first constructor holds nothing a name has
-- to pick out.
data Outcome
  = OutcomeNamed !RuleName !OwnerName
  | OutcomeRelayed !RuleName !Text
  | OutcomeAnonymous !Text !Text
