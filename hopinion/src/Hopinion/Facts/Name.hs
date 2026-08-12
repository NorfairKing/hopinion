{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

-- | What the things in a repository are called.
--
-- Every one is a newtype rather than a type synonym, so that a module name
-- cannot be passed where a declaration name is wanted, and so that the one
-- place which parses each of them is the only place that can make one.
module Hopinion.Facts.Name
  ( relPathText,
    relPathCodec,
    ModuleKey (..),
    DeclName (..),
    PackageName (..),
    ComponentName (..),
    TypeHead (..),
  )
where

import Autodocodec
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import GHC.Generics (Generic)
import Path (File, Path, Rel, parseRelFile, toFilePath)
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)
import Web.PathPieces (PathPiece)

-- | Where a file the tool read is, always inside the repository under analysis.
--
-- @Path Rel File@ itself, because it already cannot be an absolute path or a
-- directory, and a newtype over it would only be a second name for what it
-- already refuses.
relPathText :: Path Rel File -> Text
relPathText = T.pack . toFilePath

-- | A codec value rather than a 'HasCodec' instance, which for a type from
-- another package would have to be an orphan.
relPathCodec :: JSONCodec (Path Rel File)
relPathCodec =
  named "RelPath" $
    bimapCodec
      (maybe (Left "not a repository-relative file path") Right . parseRelFile . T.unpack)
      (T.pack . toFilePath)
      codec

newtype ModuleKey = ModuleKey {moduleKeyText :: Text}
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving newtype (PersistField, PersistFieldSql)
  deriving (FromJSON, ToJSON) via (Autodocodec ModuleKey)

instance Validity ModuleKey

instance HasCodec ModuleKey where
  codec = named "ModuleKey" (dimapCodec ModuleKey moduleKeyText codec)

newtype DeclName = DeclName {declNameText :: Text}
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving newtype (PersistField, PersistFieldSql)
  deriving (FromJSON, ToJSON) via (Autodocodec DeclName)

instance Validity DeclName

instance HasCodec DeclName where
  codec = named "DeclName" (dimapCodec DeclName declNameText codec)

-- | A package is keyed on its name in the store, and persistent generates that
-- key as a newtype deriving all three, so stripping them here fails the
-- quasi-quoter rather than anything a person can see. The four names beside
-- this one are in no natural key and carry none of them.
newtype PackageName = PackageName {packageNameText :: Text}
  deriving stock (Show, Read, Eq, Ord, Generic)
  -- The three web ones are persistent's doing, not this tool's: a package name
  -- is the key of a stored package, and the quasi-quoter derives them for every
  -- key type it makes. Nothing here serves a request. Removing them stops
  -- "Hopinion.Store" compiling.
  deriving newtype (PersistField, PersistFieldSql, PathPiece, ToHttpApiData, FromHttpApiData)
  deriving (FromJSON, ToJSON) via (Autodocodec PackageName)

instance Validity PackageName

instance HasCodec PackageName where
  codec = named "PackageName" (dimapCodec PackageName packageNameText codec)

-- | A component's own name, as the cabal file spells it: @lib@ for a library,
-- and the declared name for a sub-library, executable, test suite or benchmark.
--
-- What makes a module identifiable, where a module name does not: every
-- @main-is@ component has a module called @Main@, and keying on the module name
-- alone lets one of them stand in for the others.
newtype ComponentName = ComponentName {componentNameText :: Text}
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving newtype (PersistField, PersistFieldSql)
  deriving (FromJSON, ToJSON) via (Autodocodec ComponentName)

-- | No colon, which is true of every name cabal will accept and of the one this
-- tool writes for a main library. Said here because a module's identity is
-- stored as @component:module@ in one column, so the colon that separates the
-- halves has to be the first one.
instance Validity ComponentName where
  validate cn =
    mconcat
      [ genericValidate cn,
        declare "the name holds no colon" (not (T.isInfixOf ":" (componentNameText cn)))
      ]

instance HasCodec ComponentName where
  codec = named "ComponentName" (dimapCodec ComponentName componentNameText codec)

-- | The head type constructor, applied arguments dropped. Matching on the head
-- answers "is this type tested at all" rather than "is this instantiation
-- tested", which is the question the obligation engine wants.
newtype TypeHead = TypeHead {typeHeadText :: Text}
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving newtype (PersistField, PersistFieldSql)

instance Validity TypeHead
