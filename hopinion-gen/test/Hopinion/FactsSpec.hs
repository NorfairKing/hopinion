{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Hopinion.FactsSpec (spec) where

import Data.Char (isAlphaNum, isUpper)
import Data.Text (Text)
import qualified Data.Text as T
import Hopinion.Comment
import Hopinion.Facts
import Hopinion.Facts.Gen ()
import Hopinion.Rule.Id
import Test.Syd
import Test.Syd.Validity
import Test.Syd.Validity.Aeson

spec :: Spec
spec = do
  describe "RuleId" $ do
    genValidSpec @RuleId
    jsonSpec @RuleId
    it "round trips through its text" $
      forAllValid $ \r ->
        parseRuleId (ruleIdText r) `shouldBe` Just r
    -- A suppression is @[allow:RuleId] reason@, and the bracket is meant to be
    -- able to grow a second colon-separated field later. That only stays
    -- possible while no rule id contains a colon of its own.
    it "is PascalCase, so a rule id can never be mistaken for anything else" $
      forAllValid $ \r ->
        ruleIdText r `shouldSatisfy` isPascalCase
    it "contains no colon, leaving the second colon free for later" $
      forAllValid $ \r ->
        ruleIdText r `shouldSatisfy` not . T.isInfixOf ":"
  describe "Level" $ do
    genValidSpec @Level
  describe "ModuleKey" $ do
    genValidSpec @ModuleKey
    jsonSpec @ModuleKey
  describe "DeclName" $ do
    genValidSpec @DeclName
    jsonSpec @DeclName
  describe "PackageName" $ do
    genValidSpec @PackageName
    jsonSpec @PackageName
  describe "TypeHead" $ do
    genValidSpec @TypeHead
  describe "NonEmptyText" $ do
    genValidSpec @NonEmptyText
    jsonSpec @NonEmptyText
  describe "Position" $ do
    genValidSpec @Position
    jsonSpec @Position
  describe "Span" $ do
    genValidSpec @Span
    jsonSpec @Span
    -- Every fact that points at code stores its span in one column as text, so
    -- this is the boundary the whole store crosses. A path may hold a colon or
    -- a space, which is why the numbers are written first and the rest of the
    -- text is the path.
    it "roundtrips through the text a fact stores it as" $
      forAllValid $ \sp ->
        parseSpan (spanText sp) `shouldBe` Just sp
  describe "ModuleRef" $ do
    genValidSpec @ModuleRef
    jsonSpec @ModuleRef
    -- A module's identity is one column, so that a query has nothing to compare
    -- but the whole of it. This is that column.
    it "roundtrips through the text a fact stores it as" $
      forAllValid $ \ref ->
        parseModuleRef (moduleRefText ref) `shouldBe` Just ref
  describe "ScopeKey" $ do
    genValidSpec @ScopeKey
    jsonSpec @ScopeKey
  describe "ComponentKind" $ do
    genValidSpec @ComponentKind
    -- The spelling a row stores is also the spelling @--component@ accepts, so
    -- this covers the flag and the column at once.
    it "roundtrips through the text a row stores it as" $
      forAllValid $ \k ->
        parseComponentKind (componentKindText k) `shouldBe` Just k
  describe "PackageRole" $
    -- Exhaustive rather than a property: there are two of them, and what this
    -- catches is two roles rendering to the same word, which is what a third
    -- one copied from a second would be. The store keys a package on its name
    -- and carries the role beside it, so a role that reads back as the other
    -- one is a gen package judged as a main one.
    it "roundtrips through the text a row stores it as" $
      mapM_
        (\r -> parsePackageRole (packageRoleText r) `shouldBe` Just r)
        [minBound .. maxBound :: PackageRole]

  describe "ComponentName" $ do
    genValidSpec @ComponentName
    jsonSpec @ComponentName
  describe "ParseOutcome" $ do
    genValidSpec @ParseOutcome
    jsonSpec @ParseOutcome
  describe "DeclKind" $ do
    genValidSpec @DeclKind
  describe "DeclFact" $ do
    genValidSpec @DeclFact
  describe "InstanceMethods" $ do
    genValidSpec @InstanceMethods
  describe "InstanceOrigin" $ do
    genValidSpec @InstanceOrigin
  describe "InstanceFact" $ do
    genValidSpec @InstanceFact
  describe "CommentStyle" $ do
    genValidSpec @CommentStyle
    jsonSpec @CommentStyle
  describe "Attachment" $ do
    genValidSpec @Attachment
    jsonSpec @Attachment
  describe "CommentFact" $ do
    genValidSpec @CommentFact
    jsonSpec @CommentFact
  describe "AnnotationPrecision" $ do
    genValidSpec @AnnotationPrecision
    jsonSpec @AnnotationPrecision
  describe "Reason" $ do
    genValidSpec @Reason
    jsonSpec @Reason
  describe "AnnotationFact" $ do
    genValidSpec @AnnotationFact
    jsonSpec @AnnotationFact
  describe "AnnotationProblem" $ do
    genValidSpec @AnnotationProblem
    jsonSpec @AnnotationProblem
  describe "TypeAppFact" $ do
    genValidSpec @TypeAppFact
  describe "ConcatOperand" $ do
    genValidSpec @ConcatOperand
  describe "ConcatChain" $ do
    genValidSpec @ConcatChain
  describe "TemplateHaskellUse" $ do
    genValidSpec @TemplateHaskellUse
    it "roundtrips through the text a row stores it as" $
      forAllValid $ \u ->
        parseTemplateHaskellUse (templateHaskellUseText u) `shouldBe` Just u
  describe "PackageRole" $ do
    genValidSpec @PackageRole
    it "roundtrips through the text a row stores it as" $
      forAllValid $ \r ->
        parsePackageRole (packageRoleText r) `shouldBe` Just r
  describe "FormatVersion" $ do
    genValidSpec @FormatVersion
  describe "GenPackage" $ genValidSpec @GenPackage

isPascalCase :: Text -> Bool
isPascalCase t = case T.uncons t of
  Nothing -> False
  Just (c, rest) -> isUpper c && T.all (\x -> isAlphaNum x || x == '_') rest
