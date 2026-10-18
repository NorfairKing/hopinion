{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Hopinion.Check.Project.ScalarNewtypeUnused.Fact
  ( ScalarNewtype (..),
    PrimitiveField (..),
    scalarFor,
    endsInTypeName,
    wordsOfName,
    reachablePackages,
  )
where

import Data.Char (isUpper)
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Validity
import Data.Validity.Text ()
import GHC.Generics (Generic)
import Hopinion.Facts.Name
import Hopinion.Facts.Place
import Hopinion.Facts.Primitive (Primitive)

-- | One scalar the project has given a name of its own: a declaration whose
-- values are each one primitive.
data ScalarNewtype = ScalarNewtype
  { scalarNewtypeType :: !TypeHead,
    scalarNewtypePrimitive :: !Primitive
  }
  deriving stock (Show, Eq, Generic)

instance Validity ScalarNewtype

-- | One record field written as a bare primitive, which is every field this
-- rule could have anything to say about.
--
-- The owner is carried so that the one field of a scalar newtype can be told
-- from a site that went without one: a declaration cannot have declined
-- itself.
--
-- The span is the primitive rather than the whole field, because the primitive
-- is the part a fix replaces.
data PrimitiveField = PrimitiveField
  { primitiveFieldOwner :: !TypeHead,
    primitiveFieldName :: !Text,
    primitiveFieldPrimitive :: !Primitive,
    primitiveFieldSpan :: !Span
  }
  deriving stock (Show, Eq, Generic)

instance Validity PrimitiveField

-- | Every package in the project whose main library a component builds
-- against, itself included.
--
-- What it answers is whether a type the project declares elsewhere is a fix
-- this site can apply: a field in a package that does not build against the one
-- holding the newtype cannot use it until a cabal file changes too, and asking
-- for that is a different request from the one this rule makes. Nine of the
-- nineteen findings this rule had in one repository before the walk went in
-- named a type in a package downstream of the site, where the dependency would
-- have had to run backwards.
--
-- The site's own component is the first hop, because that is the component
-- whose dependencies it may use. Every hop after it is through a package's main
-- library, because that is what depending on a package gets you. A dependency
-- on one of a package's sub-libraries is read as a dependency on the package,
-- so the hop follows its main library rather than the sub-library named, which
-- is the one place this walk answers about a component nobody asked about.
--
-- Package names outside the project are dropped rather than followed: a cabal
-- file names mostly Hackage, and what a dependency of a dependency from there
-- re-exports is not something read here.
reachablePackages ::
  [PackageName] ->
  [(PackageName, ComponentName, PackageName)] ->
  PackageName ->
  ComponentName ->
  Set PackageName
reachablePackages inProject edges from component =
  let project = S.fromList inProject
      dependenciesOf :: PackageName -> ComponentName -> Set PackageName
      dependenciesOf p c =
        S.intersection
          project
          (S.fromList [d | (p', c', d) <- edges, p' == p, c' == c])
      go :: Set PackageName -> Set PackageName -> Set PackageName
      go seen frontier =
        case S.minView frontier of
          Nothing -> seen
          Just (p, rest) ->
            let next = S.difference (dependenciesOf p mainLibraryComponent) seen
             in go (S.union seen next) (S.union rest next)
      first' = S.difference (dependenciesOf from component) (S.singleton from)
   in go (S.insert from first') first'

-- | The scalar the project has already named that this field holds, or nothing
-- when the project has no name for what it holds.
--
-- The primitive has to match, because a newtype around a @Text@ is not the fix
-- for a field written as an @Int@: a field wanting both a type and a different
-- primitive is a change nothing here can propose.
--
-- The longest name wins. A project with @Name@ and @OwnerName@ has both on the
-- end of @repoOwnerName@, and the longer one names more of the field, which is
-- the one a reader can act on.
--
-- A declaration cannot have declined itself, so the one field of a scalar
-- newtype is left out however it is spelled: @unHostname@ is how @Hostname@ is
-- taken apart rather than a site that went without it.
scalarFor :: [ScalarNewtype] -> PrimitiveField -> Maybe ScalarNewtype
scalarFor lexicon pf =
  let matching =
        [ sn
        | sn <- lexicon,
          scalarNewtypePrimitive sn == primitiveFieldPrimitive pf,
          scalarNewtypeType sn /= primitiveFieldOwner pf,
          endsInTypeName (unTypeHead (scalarNewtypeType sn)) (primitiveFieldName pf)
        ]
   in case sortOn (Down . T.length . unTypeHead . scalarNewtypeType) matching of
        (sn : _) -> Just sn
        [] -> Nothing

-- | Whether a field's name ends in a type's name, word for word.
--
-- Both names are split into words and the type's words have to be the end of
-- the field's, which is stricter than asking whether the letters are on the end
-- of it. Reading the boundary off the field's letters alone said that
-- @gitUserName@ holds a @Username@, because the letters are the same ones: what
-- tells a git author's name from a platform username is that one is two words
-- and the other is one, and only splitting both can see that.
--
-- The comparison inside a word ignores case, so @fooJSONUrl@ still holds a
-- @JsonUrl@: a word spelled in capitals is the same word.
endsInTypeName :: Text -> Text -> Bool
endsInTypeName typeName field =
  let wanted = wordsOfName typeName
      have = wordsOfName field
   in not (null wanted)
        && length wanted <= length have
        && map T.toLower (drop (length have - length wanted) have) == map T.toLower wanted

-- | A name split where a reader sees one word stop and the next begin.
--
-- A run of capitals is one word, so @JSONUrl@ is @JSON@ and @Url@ rather than
-- a word per letter, and the last capital of such a run starts the next word
-- when a lowercase letter follows it.
wordsOfName :: Text -> [Text]
wordsOfName = filter (not . T.null) . go
  where
    go :: Text -> [Text]
    go t
      | T.null t = []
      | otherwise =
          let (ups, afterUps) = T.span isUpper t
           in case T.length ups of
                -- No capital here, so a run of what is not one is a word.
                0 -> let (lows, rest) = T.break isUpper t in lows : go rest
                -- One capital starts a word that the letters after it finish.
                1 -> let (lows, rest) = T.break isUpper afterUps in (ups <> lows) : go rest
                -- Several capitals are one word rather than a word each, and
                -- the last of them starts the next word when lowercase
                -- follows: the U of JSONUrl belongs to Url.
                n ->
                  let (word, carried) = T.splitAt (n - 1) ups
                   in if T.null afterUps
                        then [ups]
                        else word : go (carried <> afterUps)
