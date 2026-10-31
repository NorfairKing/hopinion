{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Hopinion.Check.Test.NoDummyValues.Fact
  ( TestLiterals (..),
    TestLiteral (..),
    Consumer (..),
    NameAt (..),
    Definition (..),
    BindingUse (..),
  )
where

import Data.Text (Text)
import Data.Validity
import GHC.Generics (Generic)
import Hopinion.Facts.Place

-- | What a test file says about its literals, before anybody has asked the
-- build where the code that receives them is defined.
data TestLiterals = TestLiterals
  { testLiteralsLiterals :: ![TestLiteral],
    -- | Every place a binding that holds a literal is used, so that what
    -- receives the binding can be asked about as well.
    testLiteralsUses :: ![BindingUse]
  }
  deriving stock (Show, Eq, Generic)

instance Validity TestLiterals

-- | A literal a test may run on: written in its body, or in a binding it uses.
data TestLiteral = TestLiteral
  { -- | The literal as the parser prints it back, so that a finding can say
    -- which of several on one line it is about.
    testLiteralText :: !Text,
    testLiteralSpan :: !Span,
    testLiteralScope :: !ScopeKey,
    -- | The test it is in, or the binding outside every test that it is in.
    -- Literals are reported one owner at a time, so that one suppression above
    -- a test answers for everything made up in it.
    testLiteralOwner :: !Span,
    -- | What receives it, innermost first, as far as the test or binding it is
    -- written in.
    testLiteralConsumers :: ![Consumer]
  }
  deriving stock (Show, Eq, Generic)

instance Validity TestLiteral

-- | One thing a value is handed to on its way out of the expression it is
-- written in.
data Consumer
  = -- | A function or constructor it is an argument of, and the bare names
    -- among that call's other arguments: a library function handed one of the
    -- project's own functions hands the value on to it.
    ConsumerCall !NameAt ![NameAt]
  | -- | The binding the value is the whole or a part of. What receives it from
    -- there is wherever that name is used.
    ConsumerBinding !Text
  deriving stock (Show, Eq, Generic)

instance Validity Consumer

-- | A name as it is written in the source: its spelling, where it starts, and
-- whether this module defines something by that spelling.
--
-- Where it starts is what the build is asked about. Whether this module
-- defines it is what is known without one.
data NameAt = NameAt
  { nameAtText :: !Text,
    nameAtPosition :: !Position,
    nameAtDefinition :: !Definition
  }
  deriving stock (Show, Eq, Generic)

instance Validity NameAt

-- | Whether this module defines something by a name's spelling, which is as
-- much as the source says about where the name is defined.
data Definition
  = DefinitionHere
  | DefinitionElsewhere
  deriving stock (Show, Eq, Generic)

instance Validity Definition

-- | One place a binding is used, and what receives it there.
data BindingUse = BindingUse
  { bindingUseName :: !Text,
    bindingUseConsumers :: ![Consumer]
  }
  deriving stock (Show, Eq, Generic)

instance Validity BindingUse
