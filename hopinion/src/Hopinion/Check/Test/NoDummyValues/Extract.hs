{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Beside the rule rather than in the shared extraction, because nothing else
-- asks what a test body is. What is shared is how an expression is taken apart,
-- and that comes from 'Hopinion.Extract.Ghc'.
module Hopinion.Check.Test.NoDummyValues.Extract (testLiteralsOf) where

import Data.Generics (listify)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Hs
import GHC.Types.SrcLoc (unLoc)
import qualified GHC.Types.SrcLoc as SrcLoc
import GHC.Utils.Outputable (showPprUnsafe)
import Hopinion.Check.Test.NoDummyValues.Fact
import Hopinion.Extract.Ghc
import Hopinion.Facts.Decl
import Hopinion.Facts.Place
import Path (File, Path, Rel)

-- | Every literal written in the body of a test that the test runs on.
--
-- The body only, which is the argument after the description: a literal in the
-- description is the test's name, and one written outside every test is setup
-- that this rule has no view on.
--
-- Within the body, a literal in an argument that 'exemptArguments' names is not
-- a value the test runs on, so it is not one of these either.
testLiteralsOf :: Path Rel File -> ModuleRef -> [DeclFact] -> [LHsDecl GhcPs] -> [TestLiteral]
testLiteralsOf rp ref decls ds =
  [ TestLiteral
      { testLiteralText = T.pack (showPprUnsafe (unLoc l)),
        testLiteralSpan = sp,
        testLiteralScope = declScopeOf ref decls sp
      }
  | l <- listify isLiteral ds,
    let loc = getLocA l,
    any (loc `SrcLoc.isSubspanOf`) bodies,
    not (any (loc `SrcLoc.isSubspanOf`) exempt),
    let sp = spanOfSrcSpan rp loc
  ]
  where
    applications :: [(Text, [SrcLoc.SrcSpan])]
    applications = concatMap applicationsAt (listify (const True :: LHsExpr GhcPs -> Bool) ds)

    bodies :: [SrcLoc.SrcSpan]
    bodies = [body | (name, _ : body : _) <- applications, isTestName name]

    exempt :: [SrcLoc.SrcSpan]
    exempt = concatMap (uncurry exemptArguments) applications

isLiteral :: LHsExpr GhcPs -> Bool
isLiteral le = case unLoc le of
  HsLit {} -> True
  HsOverLit {} -> True
  _ -> False

-- | The sydtest functions that take a description and then a test.
isTestName :: Text -> Bool
isTestName name =
  fromMaybe name (T.stripPrefix "x" name)
    `elem` ( "prop"
               : [ T.pack (concat [base, variant])
                 | base <- ["it", "specify"],
                   variant <- ["", "WithOuter", "WithBoth", "WithAll"]
                 ]
           )

-- | The arguments of a function that hold no value the test runs on.
--
-- What a test asserts is the claim rather than an input to it, so the expected
-- side of an assertion is one of these. So is a generator's definition, which
-- is the replacement this rule asks for, and so is anything that configures
-- the test rather than feeding it: a golden file's path, a failure message, how
-- many examples to run, which file a fixture is read from, how long to wait,
-- what a temporary directory is called, which program to run with which flags.
--
-- A value written to be invalid is chosen for being invalid rather than
-- standing in for any value, so it is not a dummy either.
exemptArguments :: Text -> [SrcLoc.SrcSpan] -> [SrcLoc.SrcSpan]
exemptArguments name args
  | name `elem` wholeCallNames = args
  | T.isPrefixOf "should" name = drop 1 args
  | name `elem` firstArgumentNames = take 1 args
  | any (`T.isPrefixOf` name) ["golden", "pureGolden", "readFile", "decodeFile"] = take 1 args
  | T.isPrefixOf "with" name && T.isInfixOf "Temp" name = take 1 args
  | name `elem` ["readProcess", "readProcessWithExitCode", "callProcess", "proc"] = take 2 args
  | otherwise = []

-- | Functions whose first argument is a claim or a setting.
--
-- The needle of a search is what a test claims is in its output, and the
-- generator a property is quantified over is the replacement this rule asks for.
firstArgumentNames :: [Text]
firstArgumentNames =
  [ "expectationFailure",
    "context",
    "generate",
    "forAll",
    "forAllShrink",
    "forAllShow",
    "forAllShrinkShow",
    "forAllBlind",
    "threadDelay",
    "timeout",
    "isInfixOf",
    "isPrefixOf",
    "isSuffixOf"
  ]

-- | Functions whose every argument is a claim or a setting.
--
-- The assertions of yesod-test, which take nothing but what they assert, the
-- parts of a request that the protocol fixes, and the QuickCheck settings.
wholeCallNames :: [Text]
wholeCallNames =
  [ "shouldBeInvalid",
    "setMethod",
    "addRequestHeader",
    "statusIs",
    "bodyContains",
    "bodyNotContains",
    "bodyEquals",
    "htmlCount",
    "htmlAllContain",
    "htmlAnyContain",
    "htmlNoneContain",
    "assertHeader",
    "assertNoHeader",
    "withMaxSuccess",
    "modifyMaxSuccess",
    "modifyMaxSize",
    "modifyMaxShrinks",
    "modifyMaxDiscardRatio",
    "within"
  ]

-- | Every named function applied at this node, with where each of its arguments
-- is written.
--
-- By juxtaposition, with a @$@ before its last argument, and as an infix
-- operator. The parser leaves fixity unresolved, so the last two are read off
-- the infix spine, which is in source order.
applicationsAt :: LHsExpr GhcPs -> [(Text, [SrcLoc.SrcSpan])]
applicationsAt le =
  let (leftmost, rest) = spineOf le
   in maybe [] pure (namedApplication le)
        ++ infixApplications (leftmost : map snd rest) (map fst rest)

-- | Operands and the operators between them, which is one more operand than
-- operators.
--
-- The right operand of an operator reaches as far as the next operator that
-- binds no tighter than it could. Everything after a @$@ is its argument,
-- because nothing binds looser.
infixApplications :: [LHsExpr GhcPs] -> [LHsExpr GhcPs] -> [(Text, [SrcLoc.SrcSpan])]
infixApplications operands ops = case (operands, ops) of
  (x : after@(y : ys), op : ops') ->
    let rightOperand stops = rightOperandFrom stops y (zip ops' ys)
        here = case operatorName op of
          Just "$" ->
            [ (name, args ++ [rightOperand (const False)])
            | Just (name, args) <- [namedApplication x]
            ]
          Just name -> [(name, [getLocA x, rightOperand isLoose])]
          Nothing -> []
     in here ++ infixApplications after ops'
  _ -> []

rightOperandFrom :: (Text -> Bool) -> LHsExpr GhcPs -> [(LHsExpr GhcPs, LHsExpr GhcPs)] -> SrcLoc.SrcSpan
rightOperandFrom stops first rest =
  foldl'
    SrcLoc.combineSrcSpans
    (getLocA first)
    (map (getLocA . snd) (takeWhile (not . maybe False stops . operatorName . fst) rest))

-- | The operators an assertion's right operand ends at: those of the lowest
-- two precedences, which is where the assertions are themselves.
isLoose :: Text -> Bool
isLoose name =
  name `elem` ["$", "$!", "seq", ">>", ">>=", "=<<", "&"]
    || T.isPrefixOf "should" name

operatorName :: LHsExpr GhcPs -> Maybe Text
operatorName op = case unLoc op of
  HsVar _ n -> Just (rdrText (unLoc n))
  _ -> Nothing

-- | A name applied to arguments by juxtaposition, and where each argument is.
namedApplication :: LHsExpr GhcPs -> Maybe (Text, [SrcLoc.SrcSpan])
namedApplication = go []
  where
    go :: [SrcLoc.SrcSpan] -> LHsExpr GhcPs -> Maybe (Text, [SrcLoc.SrcSpan])
    go acc le = case unLoc (peelExpr le) of
      HsApp _ f x -> go (getLocA x : acc) f
      HsAppType _ f _ -> go acc f
      HsVar _ n -> Just (rdrText (unLoc n), acc)
      _ -> Nothing
