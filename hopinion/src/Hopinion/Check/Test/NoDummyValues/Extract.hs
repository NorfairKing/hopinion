{-# LANGUAGE OverloadedStrings #-}

-- | Beside the rule rather than in the shared extraction, because nothing else
-- asks what a test body is. What is shared is how an expression is taken apart,
-- and that comes from 'Hopinion.Extract.Ghc'.
module Hopinion.Check.Test.NoDummyValues.Extract (testLiteralsOf) where

import Data.Generics (listify)
import Data.List (nub)
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
import Hopinion.Facts.Module (isSpecFilePath)
import Hopinion.Facts.Place
import Path (File, Path, Rel)

-- | Every literal a test runs on.
--
-- That is a literal in the body of a test, which is the argument after the
-- description, or in any binding of this module the body names, followed for
-- as long as there are names to follow. Where a value is bound does not change
-- whether it was made up, so moving one out of the test does not hide it.
-- A literal in the description is the test's name, and one that no test reaches
-- is not a value any test runs on.
--
-- A literal in an argument that 'exemptArguments' names is not a value the
-- test runs on either, and a name written there is not followed.
--
-- Names are followed by spelling rather than by what they resolve to, because
-- resolving them is renaming. A local that shadows another name drags that
-- name's binding in as well, which errs towards reporting.
--
-- Nothing outside a test file, because nothing else asks and this is the most
-- expensive reading of a module there is.
testLiteralsOf :: Path Rel File -> ModuleRef -> [DeclFact] -> [LHsDecl GhcPs] -> [TestLiteral]
testLiteralsOf rp ref decls ds
  | not (isSpecFilePath rp) = []
  | otherwise =
      [ TestLiteral
          { testLiteralText = T.pack (showPprUnsafe (unLoc l)),
            testLiteralSpan = sp,
            testLiteralScope = declScopeOf ref decls sp
          }
      | l <- listify isLiteral ds,
        let loc = getLocA l,
        within reached loc,
        not (within exempt loc),
        let sp = spanOfSrcSpan rp loc
      ]
  where
    expressions :: [LHsExpr GhcPs]
    expressions = listify (const True :: LHsExpr GhcPs -> Bool) ds

    applications :: [(Text, [SrcLoc.SrcSpan])]
    applications = concatMap applicationsAt expressions

    bodies :: [SrcLoc.SrcSpan]
    bodies = [body | (name, _ : body : _) <- applications, isTestName name]

    exempt :: [SrcLoc.SrcSpan]
    exempt =
      concat
        [ concatMap (uncurry exemptArguments) applications,
          concatMap mappedSpecimens expressions,
          concatMap pairKeys expressions,
          concatMap counterStarts expressions,
          concatMap expectations expressions,
          concatMap offsets expressions
        ]

    occurrences :: [(Text, SrcLoc.SrcSpan)]
    occurrences = [(rdrText (unLoc n), getLocA e) | e <- expressions, HsVar _ n <- [unLoc e]]

    reached :: [SrcLoc.SrcSpan]
    reached = reachedFrom (bindingsOf ds) occurrences exempt bodies

within :: [SrcLoc.SrcSpan] -> SrcLoc.SrcSpan -> Bool
within regions loc = any (loc `SrcLoc.isSubspanOf`) regions

-- | The regions a test runs: the ones it starts from, and every binding a name
-- in one of them refers to, until no new binding is named.
--
-- Terminates because a region is only added once and there are finitely many
-- bindings.
reachedFrom :: [(Text, SrcLoc.SrcSpan)] -> [(Text, SrcLoc.SrcSpan)] -> [SrcLoc.SrcSpan] -> [SrcLoc.SrcSpan] -> [SrcLoc.SrcSpan]
reachedFrom bindings occurrences exempt = go
  where
    go :: [SrcLoc.SrcSpan] -> [SrcLoc.SrcSpan]
    go regions =
      let named = [n | (n, loc) <- occurrences, within regions loc, not (within exempt loc)]
          new = nub [s | (n, s) <- bindings, n `elem` named, s `notElem` regions]
       in if null new then regions else go (regions ++ new)

-- | Every name this module binds, anywhere, and where what it is bound to is
-- written: a function or a pattern binding at any depth, and a name bound in a
-- @do@ block to the result of an action.
bindingsOf :: [LHsDecl GhcPs] -> [(Text, SrcLoc.SrcSpan)]
bindingsOf ds =
  [ binding
  | lb <- listify (const True :: LHsBind GhcPs -> Bool) ds,
    binding <- case unLoc lb of
      FunBind {fun_id = n} -> [(rdrText (unLoc n), getLocA lb)]
      PatBind {pat_lhs = p} -> [(name, getLocA lb) | name <- patternNames p]
      VarBind {} -> []
      PatSynBind {} -> []
  ]
    ++ [ (name, getLocA body)
       | stmt <- listify (const True :: ExprLStmt GhcPs -> Bool) ds,
         BindStmt _ p body <- [unLoc stmt],
         name <- patternNames p
       ]

patternNames :: LPat GhcPs -> [Text]
patternNames p = [rdrText (unLoc n) | VarPat _ n <- listify (const True :: Pat GhcPs -> Bool) p]

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
-- the test rather than feeding it: a failure message, how many examples to
-- run, which file a fixture is read from, how long to wait, what a temporary
-- directory is called, which program to run with which flags.
--
-- A value written to be invalid is chosen for being invalid rather than
-- standing in for any value, so it is not a dummy either.
--
-- What a golden test renders has to be the same every run, so the values it is
-- rendered from are fixed by design rather than made up in place of any value.
-- That holds for a golden helper of a project's own as much as for sydtest's.
-- A raw query is code in another language rather than a value, and the key of
-- a JSON object is fixed by the format it is written in.
--
-- What a parser is given is a specimen of the format it reads, which the test is
-- about and which the expected value is worked out from, so it is no more a
-- dummy than the expected value is.
exemptArguments :: Text -> [SrcLoc.SrcSpan] -> [SrcLoc.SrcSpan]
exemptArguments name args
  | name `elem` wholeCallNames = args
  | T.isPrefixOf "should" name = drop 1 args
  | name == ".=" = take 1 args
  | name `elem` [".:", ".:?", ".:!"] = drop 1 args
  | name `elem` firstArgumentNames = take 1 args
  | T.isPrefixOf "golden" name || T.isInfixOf "Golden" name = args
  | any (`T.isPrefixOf` name) ["readFile", "decodeFile", "parse", "decode", "eitherDecode"] = take 1 args
  | name `elem` ["rawExecute", "rawExecuteCount", "rawSql", "rawQuery"] = take 1 args
  | T.isPrefixOf "with" name && T.isInfixOf "Temp" name = take 1 args
  | name `elem` ["readProcess", "readProcessWithExitCode", "callProcess", "proc"] = take 2 args
  | name `elem` ["readCreateProcess", "readCreateProcessWithExitCode"] = args
  | otherwise = []

-- | The keys of the pairs a JSON object or an environment is built from.
--
-- A key in either is fixed by the format or by the program that reads it, the
-- same as a key written with @.=@, while what it is paired with can still be
-- made up.
pairKeys :: LHsExpr GhcPs -> [SrcLoc.SrcSpan]
pairKeys le = case namedApplicationOf le of
  Just (name, args)
    | name == "object" || T.isPrefixOf "parserEnv" name ->
        [ getLocA key
        | arg <- args,
          tuple <- listify (const True :: LHsExpr GhcPs -> Bool) arg,
          ExplicitTuple _ (Present _ key : _) _ <- [unLoc tuple]
        ]
  _ -> []

-- | What a test names as expected: a constructor, a function or a record field
-- whose name says so, which is a claim the same way the expected side of an
-- assertion is.
expectations :: LHsExpr GhcPs -> [SrcLoc.SrcSpan]
expectations le = case unLoc le of
  RecordCon {rcon_con = con, rcon_flds = HsRecFields {rec_flds = fields}}
    | isExpectationName (rdrText (unLoc con)) -> [getLocA le]
    | otherwise ->
        [ getLocA (hfbRHS (unLoc field))
        | field <- fields,
          isExpectationName (rdrText (unLoc (foLabel (unLoc (hfbLHS (unLoc field))))))
        ]
  _ -> case namedApplication le of
    Just (name, args) | isExpectationName name -> args
    _ -> []

isExpectationName :: Text -> Bool
isExpectationName name = T.isPrefixOf "expected" name || T.isPrefixOf "Expected" name

-- | A literal added to or taken from something that is not one, as in
-- @threshold + 1@ or @length xs - 1@.
--
-- That states where a value is relative to another, which is a claim about the
-- other value rather than a value made up in its place.
offsets :: LHsExpr GhcPs -> [SrcLoc.SrcSpan]
offsets le = case unLoc le of
  OpApp _ l op r
    | operatorName op `elem` [Just "+", Just "-"] ->
        case (isLiteral (peelExpr l), isLiteral (peelExpr r)) of
          (False, True) -> [getLocA r]
          (True, False) -> [getLocA l]
          _ -> []
  _ -> []

-- | Where an open-ended enumeration starts, as in @zip [0 ..] xs@.
--
-- That is the base of a count rather than data: nothing is made up by numbering
-- from zero or one.
counterStarts :: LHsExpr GhcPs -> [SrcLoc.SrcSpan]
counterStarts le = case unLoc le of
  ArithSeq _ _ (From start) -> [getLocA start]
  _ -> []

-- | The list a function is mapped over, when what that function is given is
-- exempt.
--
-- A parser mapped over a list of specimens is given each of them in turn, so
-- they are as much specimens as one written directly after it. Written as a
-- list, they would otherwise be one statement holding several findings, which
-- no single suppression can answer for.
mappedSpecimens :: LHsExpr GhcPs -> [SrcLoc.SrcSpan]
mappedSpecimens le =
  let (leftmost, rest) = spineOf le
      prefix = case namedApplicationOf le of
        Just (name, f : xs : _) | isMapper name && takesSpecimens f -> [getLocA xs]
        _ -> []
      infix' =
        [ getLocA y
        | (x, (op, y)) <- zip (leftmost : map snd rest) rest,
          operatorName op `elem` [Just "<$>", Just "<&>"],
          takesSpecimens x
        ]
   in prefix ++ infix'
  where
    isMapper :: Text -> Bool
    isMapper name = name `elem` ["map", "fmap", "traverse", "traverse_", "mapM", "mapM_", "concatMap"]

    takesSpecimens :: LHsExpr GhcPs -> Bool
    takesSpecimens f = case namedApplication f of
      Just (name, []) -> not (null (exemptArguments name [SrcLoc.noSrcSpan]))
      _ -> False

-- | Functions whose first argument is a claim or a setting.
--
-- The needle of a search is what a test claims is in its output, and so is the
-- key of a lookup: it names something the output should hold, or repeats a
-- value written into the input, where that value is reported already. A
-- selector names a part of a page, and a form or query parameter's name is
-- fixed by the handler it is sent to.
--
-- The generator a property is quantified over is the replacement this rule
-- asks for. A QuickCheck setting or a retry delay is its first argument alone,
-- because what it wraps is a test or a policy like any other.
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
    "isSuffixOf",
    "lookup",
    "member",
    "notMember",
    "htmlQuery",
    "addPostParam",
    "addGetParam",
    "byLabelExact",
    "byLabelContain",
    "bySelectorLabelContain",
    "fileByLabelExact",
    "fileByLabelContain",
    "capDelay",
    "withMaxSuccess",
    "modifyMaxSuccess",
    "modifyMaxSize",
    "modifyMaxShrinks",
    "modifyMaxDiscardRatio",
    "within"
  ]

-- | Functions whose every argument is a claim or a setting.
--
-- The assertions of yesod-test, which take nothing but what they assert, the
-- parts of a request that the protocol fixes, and retry policies, which are
-- settings.
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
    "limitRetries",
    "constantDelay",
    "exponentialBackoff",
    "fullJitterBackoff"
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
namedApplication = fmap (fmap (map getLocA)) . namedApplicationOf

-- | A name applied to arguments by juxtaposition, and the arguments.
namedApplicationOf :: LHsExpr GhcPs -> Maybe (Text, [LHsExpr GhcPs])
namedApplicationOf = go []
  where
    go :: [LHsExpr GhcPs] -> LHsExpr GhcPs -> Maybe (Text, [LHsExpr GhcPs])
    go acc le = case unLoc (peelExpr le) of
      HsApp _ f x -> go (x : acc) f
      HsAppType _ f _ -> go acc f
      HsVar _ n -> Just (rdrText (unLoc n), acc)
      _ -> Nothing
