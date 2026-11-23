-- | Types of the module's own that happen to share a scalar's name.
module Good.Markup where

data Text = Plain | Bold Text

type Heading = Text

data Word = Noun | Verb

type Token = Word
