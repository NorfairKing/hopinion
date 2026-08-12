{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The types. Everything else in hopinion is a function between them.
--
-- These cross a process boundary between the package layer and the project
-- layer, so they carry codecs and a version stamp. Producer and consumer are
-- the same binary within one evaluation, so the version fields turn a mismatch
-- into a loud failure rather than reading old facts.
--
-- **A type belongs to whatever gives it meaning, and lives here only when
-- nothing does.** A rule's own facts are in that rule's module, in the table it
-- brought; what a comment is about is in 'Hopinion.Comment'. What is left is
-- what extraction produces and more than one thing reads. The suppression types
-- are here for a duller reason: 'Hopinion.Annotation' needs 'Finding', and
-- moving them there would make the two modules import each other.
module Hopinion.Facts
  ( module Hopinion.Facts.Name,
    module Hopinion.Facts.Place,
    module Hopinion.Facts.Decl,
    module Hopinion.Facts.Suppression,
    ComponentKind (..),
    ParseOutcome (..),
    InstanceOrigin (..),
    InstanceFact (..),
    NameFact (..),
    TypeAppFact (..),
    TemplateHaskellUse (..),
    CompiledModule (..),
    DeclaredInstance (..),
    ModuleContext (..),
    moduleContextRef,
    PackageRole (..),
    GenPackage (..),
    FormatVersion (..),
    currentFormatVersion,
    componentKindText,
    parseComponentKind,
    packageRoleText,
    parsePackageRole,
    templateHaskellUseText,
    parseTemplateHaskellUse,
  )
where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy as LB
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Validity
import Data.Validity.Aeson ()
import Data.Validity.Containers ()
import Data.Validity.Path ()
import Data.Validity.Text ()
import Database.Persist (PersistField (..))
import Database.Persist.Sql (PersistFieldSql (..), SqlType (..))
import GHC.Generics (Generic)
import Hopinion.Comment (CommentFact (..))
import Hopinion.Facts.Decl
import Hopinion.Facts.Name
import Hopinion.Facts.Persist
import Hopinion.Facts.Place
import Hopinion.Facts.Suppression
import Hopinion.Hie (CompiledModule (..), DeclaredInstance (..))
import Path (File, Path, Rel)

data ComponentKind
  = ComponentLib
  | ComponentApp
  | ComponentTest
  | ComponentBench
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

instance Validity ComponentKind

-- | Kept in the facts rather than thrown away, because the project layer must
-- treat an unparsed module as an error rather than as a module with no
-- declarations.
data ParseOutcome
  = ParsedOk
  | ParseFailed !Position !Text
  | -- | The module is real and its source is not Haskell: a preprocessor
    -- generates it at build time. Neither a parse failure nor a module that
    -- vanished, so it must be neither reported nor silently dropped.
    NotHaskellSource
  deriving stock (Show, Eq, Generic)
  deriving (FromJSON, ToJSON) via (Autodocodec ParseOutcome)

instance Validity ParseOutcome

instance HasCodec ParseOutcome where
  codec =
    named "ParseOutcome" $
      dimapCodec fromEither toEither $
        disjointEitherCodec
          (stringConstCodec ((ParsedOk, "ok") :| [(NotHaskellSource, "not-haskell-source")]))
          ( object "ParseFailed" $
              (,)
                <$> requiredField "loc" "where the parse failed" .= fst
                <*> requiredField "message" "why the parse failed" .= snd
          )
    where
      fromEither = either id (uncurry ParseFailed)
      toEither = \case
        ParsedOk -> Left ParsedOk
        NotHaskellSource -> Left NotHaskellSource
        ParseFailed l m -> Right (l, m)

-- | A closed sum on purpose: a further form forces every site to be
-- reconsidered rather than falling into a catch-all, and a missed form is a
-- silently unsatisfied obligation.
--
-- 'OriginDerivingUnspecified' is a deriving clause with no strategy. Which one
-- GHC picks depends on the extensions in force, so recording it as stock would
-- be a guess.
data InstanceOrigin
  = OriginInstanceDecl
  | OriginStandaloneDeriving
  | OriginDerivingStock
  | OriginDerivingNewtype
  | OriginDerivingAnyclass
  | OriginDerivingVia !TypeHead
  | OriginDerivingUnspecified
  deriving stock (Show, Eq, Ord, Generic)

instance Validity InstanceOrigin

data InstanceFact = InstanceFact
  { instanceFactClass :: !Text,
    instanceFactType :: !TypeHead,
    instanceFactOrigin :: !InstanceOrigin,
    instanceFactSpan :: !Span,
    -- | Where an annotation about this instance has to go, which is also where
    -- the finding is reported.
    instanceFactScope :: !ScopeKey
  }
  deriving stock (Show, Eq, Generic)

instance Validity InstanceFact

data TypeAppFact = TypeAppFact
  { typeAppFactFunction :: !Text,
    typeAppFactHead :: !TypeHead,
    typeAppFactSpan :: !Span
  }
  deriving stock (Show, Eq, Generic)

instance Validity TypeAppFact

-- | A parser cannot see what Template Haskell generates, so a module that uses
-- it records the fact, and the rules that care put the module to the compiler
-- rather than concluding that what they cannot see is not there.
--
-- Which of the two forms it is, because they hide different amounts: a splice
-- runs arbitrary code and what comes back appears nowhere in the file, where a
-- quasiquote expands one body that is in the file. A module doing both records
-- the splice, which is the weaker guarantee.
data TemplateHaskellUse
  = NoTemplateHaskell
  | UsesQuasiQuotes
  | UsesSplices
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

instance Validity TemplateHaskellUse

-- | Which module this is, in which component, which is what a scope names.
moduleContextRef :: ModuleContext -> ModuleRef
moduleContextRef ctx =
  ModuleRef
    { moduleRefComponent = moduleContextComponentName ctx,
      moduleRefModule = moduleContextModule ctx
    }

-- | A capitalised name as the module's own tokens have it, and where an
-- annotation about it has to go.
--
-- Read off the tokens rather than off the parse tree, because the tokens are
-- where a name written in the code is already told apart from the same word in
-- a comment, in a string literal, or in the tail of a module name.
data NameFact = NameFact
  { nameFactText :: !Text,
    nameFactSpan :: !Span,
    nameFactScope :: !ScopeKey
  }
  deriving stock (Show, Eq, Generic)

instance Validity NameFact

-- | Everything the parser saw about one module.
--
-- Never serialised, and that is the point: a rule that reads one runs in the
-- process that produced it, so a fact it turns straight into a finding is never
-- written down. What a rule does have to carry, that rule says.
data ModuleContext = ModuleContext
  { moduleContextModule :: !ModuleKey,
    moduleContextPath :: !(Path Rel File),
    moduleContextComponent :: !ComponentKind,
    -- | Which component, by name, so that two modules GHC both calls Main are
    -- two modules here as well.
    moduleContextComponentName :: !ComponentName,
    moduleContextInstances :: ![InstanceFact],
    moduleContextNames :: ![NameFact],
    moduleContextComments :: ![CommentFact],
    moduleContextAnnotations :: ![AnnotationFact],
    moduleContextAnnotationProblems :: ![AnnotationProblem],
    moduleContextTypeApps :: ![TypeAppFact],
    moduleContextTemplateHaskell :: !TemplateHaskellUse,
    moduleContextOutcome :: !ParseOutcome
  }
  deriving stock (Show, Eq, Generic)

data PackageRole
  = RoleMain
  | RoleGen
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

instance Validity PackageRole

newtype FormatVersion = FormatVersion {formatVersionNumber :: Word}
  deriving stock (Show, Eq, Ord, Generic)

instance Validity FormatVersion

-- | Bumped whenever a store this tool writes stops being one an older build of
-- it could read: a column added, a column reinterpreted, a table renamed.
--
-- The only version the store carries. A tool version beside it could not work:
-- the stamp is keyed on the format, so a merged store's differing tool version
-- would be dropped by the insert, and two versions where one is never read is a
-- check that reads as stronger than it is.
currentFormatVersion :: FormatVersion
currentFormatVersion = FormatVersion 4

-- | Where a test for some package lives, and where it would live if it lived
-- anywhere: both carry the name, because a package with nowhere to put its
-- tests has all its obligations unmet, and saying which package is missing is
-- most of what the reader needs.
data GenPackage
  = NoGenPackage !PackageName
  | GenPackage !PackageName
  deriving stock (Show, Eq, Generic)

instance Validity GenPackage

-- | The enum-shaped envelope types go to the database as the spelling they
-- already have, rather than through Show and Read, so a stored row stays
-- readable and a renamed constructor cannot silently change what was written.
instance PersistField ComponentKind where
  toPersistValue = toPersistValue . componentKindText
  fromPersistValue v = do
    t <- fromPersistValue v
    maybe (Left "not a component kind") Right (parseComponentKind t)

instance PersistFieldSql ComponentKind where
  sqlType _ = SqlString

instance PersistField PackageRole where
  toPersistValue = toPersistValue . packageRoleText
  fromPersistValue v = do
    t <- fromPersistValue v
    maybe (Left "not a package role") Right (parsePackageRole t)

instance PersistFieldSql PackageRole where
  sqlType _ = SqlString

instance PersistField TemplateHaskellUse where
  toPersistValue = toPersistValue . templateHaskellUseText
  fromPersistValue v = do
    t <- fromPersistValue v
    maybe (Left "not a Template Haskell use") Right (parseTemplateHaskellUse t)

instance PersistFieldSql TemplateHaskellUse where
  sqlType _ = SqlString

-- | Both of these have a shape rather than a spelling, and nothing queries into
-- one, so they are stored whole through the codec they already carry.
instance PersistField ParseOutcome where
  toPersistValue = toPersistValue . TE.decodeUtf8 . LB.toStrict . JSON.encode . toJSONViaCodec
  fromPersistValue = fromPersistValueViaCodec

instance PersistFieldSql ParseOutcome where
  sqlType _ = SqlString

componentKindText :: ComponentKind -> Text
componentKindText = \case
  ComponentLib -> "lib"
  ComponentApp -> "app"
  ComponentTest -> "test"
  ComponentBench -> "bench"

-- | What @--component@ accepts as well as what a row stores, so the spellings a
-- person can type are the spellings the store uses and there is one table of
-- them rather than two.
parseComponentKind :: Text -> Maybe ComponentKind
parseComponentKind t = lookup t [(componentKindText k, k) | k <- [minBound .. maxBound]]

packageRoleText :: PackageRole -> Text
packageRoleText = \case
  RoleMain -> "main"
  RoleGen -> "gen"

parsePackageRole :: Text -> Maybe PackageRole
parsePackageRole t = lookup t [(packageRoleText r, r) | r <- [minBound .. maxBound]]

templateHaskellUseText :: TemplateHaskellUse -> Text
templateHaskellUseText = \case
  NoTemplateHaskell -> "no"
  UsesQuasiQuotes -> "quasiquotes"
  UsesSplices -> "splices"

parseTemplateHaskellUse :: Text -> Maybe TemplateHaskellUse
parseTemplateHaskellUse t =
  lookup t [(templateHaskellUseText u, u) | u <- [minBound .. maxBound]]
