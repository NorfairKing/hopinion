{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Hopinion.Check.Test.NoDummyValues.Fact
  ( TestLiterals (..),
    TestLiteral (..),
    Consumer (..),
    NameAt (..),
    Definition (..),
    BindingUse (..),
    ParameterUse (..),
    FunctionArity (..),
    LambdaUse (..),
    HandedName (..),
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
    testLiteralsUses :: ![BindingUse],
    -- | What receives each parameter of the functions this module defines, so
    -- that a value handed to one can be followed into it.
    testLiteralsParameters :: ![ParameterUse],
    -- | How many parameters each function this module defines takes, so that
    -- one a value is handed to but never used is known to be unused rather
    -- than unknown.
    testLiteralsArities :: ![FunctionArity],
    -- | Where each name this module binds is written, so that a name the build
    -- says is local can be told apart from a parameter: a function the test is
    -- handed, as in a library's callback, is no code of this module's.
    testLiteralsBinders :: ![Position],
    -- | Every use, inside a lambda handed to a function, of a name the
    -- lambda's parameters bind, so that what a list is handed on to element
    -- by element can be followed.
    testLiteralsLambdaUses :: ![LambdaUse],
    -- | Every bare name handed to a function as an argument, for the same.
    testLiteralsHandedNames :: ![HandedName]
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
  = -- | A function or constructor it is an argument of, and which argument it
    -- is, counting from nought.
    ConsumerCall !NameAt !Word
  | -- | A list the value is an element of. A function handed to the same call
    -- as the list may be handed each element in turn.
    ConsumerElement
  | -- | The binding the value is the whole or a part of. What receives it from
    -- there is wherever that name is used.
    ConsumerBinding !NameAt
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
    -- | Where the use is written, which the build can say the binder of.
    bindingUseAt :: !Position,
    bindingUseConsumers :: ![Consumer]
  }
  deriving stock (Show, Eq, Generic)

instance Validity BindingUse

-- | One place a parameter of a function this module defines is used inside
-- that function, and what receives it there.
data ParameterUse = ParameterUse
  { parameterUseFunction :: !Text,
    -- | Where the function's binder is written.
    parameterUseFunctionAt :: !Position,
    -- | Which parameter, counting from nought.
    parameterUseIndex :: !Word,
    -- | Where the parameter's binder is written, and where this use of it is,
    -- so that a use of another name spelled the same is told apart.
    parameterUseBinderAt :: !Position,
    parameterUseAt :: !Position,
    parameterUseConsumers :: ![Consumer]
  }
  deriving stock (Show, Eq, Generic)

instance Validity ParameterUse

-- | One use of a lambda's parameter, in a lambda that is an argument of a
-- call.
data LambdaUse = LambdaUse
  { -- | Where the head of the call the lambda is an argument of is written.
    lambdaUseCallAt :: !Position,
    lambdaUseBinderAt :: !Position,
    lambdaUseAt :: !Position,
    lambdaUseConsumers :: ![Consumer]
  }
  deriving stock (Show, Eq, Generic)

instance Validity LambdaUse

-- | A bare name that is an argument of a call.
data HandedName = HandedName
  { -- | Where the head of the call is written.
    handedNameCallAt :: !Position,
    handedNameName :: !NameAt
  }
  deriving stock (Show, Eq, Generic)

instance Validity HandedName

-- | A function this module defines and how many parameters it names.
data FunctionArity = FunctionArity
  { functionArityName :: !Text,
    functionArityAt :: !Position,
    functionArityParameters :: !Word
  }
  deriving stock (Show, Eq, Generic)

instance Validity FunctionArity
