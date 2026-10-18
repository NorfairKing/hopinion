module Good where

import Data.Text (Text)
import Path (Abs, File, Path)

-- The type names it, which is the shape the rule exists to ask for.
newtype OwnerName = OwnerName Text

newtype RuleName = RuleName Text

-- A field name is a name too, so a wrapper written with record syntax is as
-- named as one written without.
newtype Reason = Reason {unReason :: Text}

-- One constructor holding one field is a newtype with more keystrokes, and the
-- type names the field either way.
data CommitMessage = CommitMessage Text

-- What the rule asks for: every scalar a type, and nothing left to position.
data OIDCRuleError
  = OIDCRuleCacheOwnerHasNoShape !RuleName !OwnerName
  | -- And beside it the residue, written down rather than hidden. A reason
    -- somebody else wrote is prose and wrapping it would name nothing, but it
    -- sits beside a field the constructor name could equally have been about,
    -- so the rule cannot tell this from the shape it is for. What settles it is
    -- the reason here, which is the whole of what an annotation is for.
    --
    -- [allow:HsNoAnonymousScalar] The Text is what the resolver said, relayed.
    OIDCRuleUnbounded !RuleName !Text

-- A constructor of one field is named by its own name: a reason travelling
-- alone has nothing beside it for that name to be ambiguous between, and it is
-- the shape every error type in this repository is written in.
data Answer a
  = Answered !a
  | AnswerUnusable ![Text]

-- Record fields are names, so a record is the other half of the rule rather
-- than a violation of it. Whether these two want types is a judgement nothing
-- here can make, and it is not this rule's to make: what it refuses is a scalar
-- a reader can only find by counting.
data Settings = Settings
  { settingsHost :: !Text,
    settingsUser :: !Text
  }

-- Two Bools in one constructor is HsNoDomainBool's finding. The same line
-- reported twice in two vocabularies is worse than the same line reported once.
data Update = UpdateJobCompleted !Bool !(Maybe Bool)

-- Two of these are as swappable as two Texts, and the rule is still silent: a
-- type that takes arguments is not a primitive, and the newtype a finding would
-- ask for is what these already are.
data Copy = Copy !(Path Abs File) !(Path Abs File)

-- A synonym is a name, and the one thing wrong with it belongs to
-- HsNewtypeNotSynonym. Saying it again here would be the same complaint about
-- the same line in another vocabulary, and the rule that already makes it is
-- what keeps this from being a way around this one.
type Hostname = Text

data Endpoint = Endpoint !Hostname !Hostname

-- Nothing positional to count.
data Mode
  = ModeRead
  | ModeWrite
