{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hopinion.Facts.Package
  ( PackageRole (..),
    packageRoleText,
    parsePackageRole,
    GenPackage (..),
  )
where

import Data.Text (Text)
import Data.Validity
import Database.Persist (PersistField (..))
import Database.Persist.Sql (PersistFieldSql (..), SqlType (..))
import GHC.Generics (Generic)
import Hopinion.Facts.Name

data PackageRole
  = RoleMain
  | RoleGen
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

instance Validity PackageRole

-- | Stored as the spelling it already has rather than through Show and Read, so
-- a stored row stays readable and a renamed constructor cannot silently change
-- what was written.
instance PersistField PackageRole where
  toPersistValue = toPersistValue . packageRoleText
  fromPersistValue v = do
    t <- fromPersistValue v
    maybe (Left "not a package role") Right (parsePackageRole t)

instance PersistFieldSql PackageRole where
  sqlType _ = SqlString

packageRoleText :: PackageRole -> Text
packageRoleText = \case
  RoleMain -> "main"
  RoleGen -> "gen"

parsePackageRole :: Text -> Maybe PackageRole
parsePackageRole t = lookup t [(packageRoleText r, r) | r <- [minBound .. maxBound]]

-- | Where a test for some package lives, and where it would live if it lived
-- anywhere: both carry the name, because a package with nowhere to put its
-- tests has all its obligations unmet, and saying which package is missing is
-- most of what the reader needs.
data GenPackage
  = NoGenPackage !PackageName
  | GenPackage !PackageName
  deriving stock (Show, Eq, Generic)

instance Validity GenPackage
