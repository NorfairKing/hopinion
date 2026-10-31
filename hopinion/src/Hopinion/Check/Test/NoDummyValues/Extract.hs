{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Beside the rule rather than in the shared extraction, because nothing else
-- asks what a test body is. What is shared is how an expression is taken apart,
-- and that comes from 'Hopinion.Extract.Ghc'.
module Hopinion.Check.Test.NoDummyValues.Extract (testLiteralsOf) where

import Data.Data (Data)
import Data.Generics (listify)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Data.FastString (unpackFS)
import GHC.Hs
import GHC.Types.Name.Reader (RdrName)
import GHC.Types.SourceText (IntegralLit (..))
import GHC.Types.SrcLoc (unLoc)
import qualified GHC.Types.SrcLoc as SrcLoc
import GHC.Utils.Outputable (showPprUnsafe)
import Hopinion.Check.Test.NoDummyValues.Fact
import Hopinion.Extract.Ghc
import Hopinion.Facts.Decl
import Hopinion.Facts.Module (isSpecFilePath)
import Hopinion.Facts.Place
import Language.Haskell.Syntax.Pat (LHsRecUpdField)
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
            testLiteralScope = declScopeOf ref decls sp,
            testLiteralOwner = spanOfSrcSpan rp owner
          }
      | l <- listify isLiteral ds,
        not (isSqlLiteral l),
        let loc = getLocA l,
        within reached loc,
        not (within exempt loc),
        not (within unnamedLets loc),
        Just owner <- [ownerOf loc],
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
          concatMap offsets expressions,
          concatMap comparisons expressions,
          concatMap sectionsAndCounters expressions,
          protocolFieldKeys ds,
          constructorTables ds
        ]

    occurrences :: [(Text, SrcLoc.SrcSpan)]
    occurrences = [(rdrText (unLoc n), getLocA e) | e <- expressions, HsVar _ n <- [unLoc e]]

    reached :: [SrcLoc.SrcSpan]
    reached = reachedFrom (bindingsOf ds) occurrences exempt letsInTests bodies

    -- A let inside a test runs only if something uses it, so what it holds is
    -- only run on once a name outside an exempt position reaches it. One that
    -- only an exempt position names, as in @let ctx = ... in context ctx@, is
    -- as exempt as that position. A name bound to an action's result is not
    -- one of these, because the action runs whether or not the name is used.
    letsInTests :: [SrcLoc.SrcSpan]
    letsInTests =
      nub
        [ getLocA lb
        | lb <- listify (const True :: LHsBind GhcPs -> Bool) ds,
          any (\b -> getLocA lb `SrcLoc.isSubspanOf` b && getLocA lb /= b) bodies
        ]

    unnamedLets :: [SrcLoc.SrcSpan]
    unnamedLets = [l | l <- letsInTests, l `notElem` reached]

    -- The innermost test a literal is in, and the outermost binding it was
    -- reached through when it is in none, so that a binding with lets or a
    -- comprehension inside it is still one thing to answer for.
    ownerOf :: SrcLoc.SrcSpan -> Maybe SrcLoc.SrcSpan
    ownerOf loc = case innermost [b | b <- bodies, loc `SrcLoc.isSubspanOf` b] of
      Just body -> Just body
      Nothing -> outermost [r | r <- reached, loc `SrcLoc.isSubspanOf` r]

-- | The region that contains every other one of these, given regions that all
-- contain one point and so nest.
outermost :: [SrcLoc.SrcSpan] -> Maybe SrcLoc.SrcSpan
outermost = \case
  [] -> Nothing
  (r : rs) -> Just (foldl' (\outer s -> if outer `SrcLoc.isSubspanOf` s then s else outer) r rs)

-- | The region every other one of these contains, given regions that all
-- contain one point and so nest.
innermost :: [SrcLoc.SrcSpan] -> Maybe SrcLoc.SrcSpan
innermost = \case
  [] -> Nothing
  (r : rs) -> Just (foldl' (\inner s -> if s `SrcLoc.isSubspanOf` inner then s else inner) r rs)

within :: [SrcLoc.SrcSpan] -> SrcLoc.SrcSpan -> Bool
within regions loc = any (loc `SrcLoc.isSubspanOf`) regions

-- | The regions a test runs: the ones it starts from, and every binding a name
-- in one of them refers to, until no new binding is named.
--
-- A let it is given is inside a region already, and is run only once a name
-- reaches it, so until then nothing written in it names anything.
--
-- Terminates because a region is only added once and there are finitely many
-- bindings.
reachedFrom ::
  [(Text, SrcLoc.SrcSpan)] ->
  [(Text, SrcLoc.SrcSpan)] ->
  [SrcLoc.SrcSpan] ->
  [SrcLoc.SrcSpan] ->
  [SrcLoc.SrcSpan] ->
  [SrcLoc.SrcSpan]
reachedFrom bindings occurrences exempt lets = go
  where
    go :: [SrcLoc.SrcSpan] -> [SrcLoc.SrcSpan]
    go regions =
      let unnamed = [l | l <- lets, l `notElem` regions]
          named =
            [ n
            | (n, loc) <- occurrences,
              within regions loc,
              not (within exempt loc),
              not (within unnamed loc)
            ]
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

-- | A string literal that is a SQL statement, whatever runs it.
--
-- A statement is code in another language rather than a value, the same as one
-- handed to 'rawSql', and a project's own runner for it is as likely as
-- persistent's.
isSqlLiteral :: LHsExpr GhcPs -> Bool
isSqlLiteral le = case unLoc le of
  HsLit _ (HsString _ s) -> isSqlText (T.pack (unpackFS s))
  HsOverLit _ OverLit {ol_val = HsIsString _ s} -> isSqlText (T.pack (unpackFS s))
  _ -> False

isSqlText :: Text -> Bool
isSqlText t = case T.words (T.stripStart t) of
  (keyword : _ : _) -> keyword `elem` ["SELECT", "INSERT", "UPDATE", "DELETE", "WITH", "CREATE", "DROP", "ALTER", "LISTEN", "NOTIFY", "DO", "BEGIN", "COMMIT", "ROLLBACK", "SET", "TRUNCATE", "EXPLAIN"]
  _ -> False

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
-- A local assertion helper named for what it asserts, like
-- @runStatusShouldBe now run expected@, takes what it expects last.
--
-- What a golden test renders has to be the same every run, so the values it is
-- rendered from are fixed by design rather than made up in place of any value.
-- That holds for a golden helper of a project's own as much as for sydtest's.
-- A timeout is its duration, and what it wraps is a test like any other.
--
-- How many threads a test runs at once, and how many times it repeats an action
-- across them, are settings of the test rather than values it runs on.
--
-- A raw query is code in another language rather than a value, whether
-- persistent runs it or postgresql-simple does, after its connection. The key of
-- a JSON object is fixed by the format it is written in.
--
-- An opt-env-conf parser test is given a specimen and the value it should
-- parse to, so neither half is made up.
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
  | T.isInfixOf "Timeout" name || T.isInfixOf "timeout" name = take 1 args
  | any (`T.isSuffixOf` name) ["ShouldBe", "ShouldNotBe", "ShouldContain", "ShouldNotContain", "ShouldReturn", "ShouldSatisfy"] = drop (length args - 1) args
  | (T.isPrefixOf "parser" name || T.isInfixOf "Parser" name) && T.isSuffixOf "Test" name = args
  | name `elem` ["pooledReplicateConcurrentlyN", "pooledReplicateConcurrentlyN_"] = take 2 args
  | T.isPrefixOf "pooled" name && T.isInfixOf "ConcurrentlyN" name = take 1 args
  | name `elem` ["replicateConcurrently", "replicateConcurrently_"] = take 1 args
  | any (`T.isPrefixOf` name) ["readFile", "decodeFile", "parse", "decode", "eitherDecode"] = take 1 args
  | name `elem` ["rawExecute", "rawExecuteCount", "rawSql", "rawQuery"] = take 1 args
  | name `elem` ["execute", "execute_", "query", "query_"] = take 1 (drop 1 args)
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
    | name == "object" || T.isPrefixOf "parserEnv" name || isProtocolPairName name ->
        [ getLocA key
        | arg <- args,
          tuple <- listify (const True :: LHsExpr GhcPs -> Bool) arg,
          ExplicitTuple _ (Present _ key : _) _ <- [unLoc tuple]
        ]
  _ -> []

-- | The literals a table from constructors to values maps to, as in a @\\case@
-- giving each constructor of a sum type its protocol code.
--
-- A mapping that takes every case apart by its constructor is a specification
-- of what each one means, written down once, rather than a value made up for a
-- test.
constructorTables :: [LHsDecl GhcPs] -> [SrcLoc.SrcSpan]
constructorTables ds =
  [ getLocA body
  | mg <- listify (const True :: MatchGroup GhcPs (LHsExpr GhcPs) -> Bool) ds,
    let alternatives = map unLoc (unLoc (mg_alts mg)),
    length alternatives > 1,
    Just bodies <- [traverse constructorToLiteral alternatives],
    body <- bodies
  ]
  where
    constructorToLiteral :: Match GhcPs (LHsExpr GhcPs) -> Maybe (LHsExpr GhcPs)
    constructorToLiteral m = case (map unLoc (unLoc (m_pats m)), m_grhss m) of
      ([p], GRHSs {grhssGRHSs = [lgrhs]}) | isConstructorPattern p -> case unLoc lgrhs of
        GRHS _ [] body | isLiteral (peelExpr body) -> Just body
        _ -> Nothing
      _ -> Nothing

    isConstructorPattern :: Pat GhcPs -> Bool
    isConstructorPattern = \case
      ConPat {} -> True
      ParPat _ p -> isConstructorPattern (unLoc p)
      _ -> False

-- | Whether a name says it holds HTTP headers or a query string, whose pairs
-- start with a name the protocol or the handler fixes.
isProtocolPairName :: Text -> Bool
isProtocolPairName name = any (`T.isInfixOf` name) ["Header", "header", "QueryString", "queryString"]

-- | The names in the pairs a record field of headers or a query string is set
-- to, as in @request {requestHeaders = [("Accept", value)]}@.
protocolFieldKeys :: [LHsDecl GhcPs] -> [SrcLoc.SrcSpan]
protocolFieldKeys ds =
  concat
    [ [keySpan | field <- listify (const True :: HsFieldBind (LFieldOcc GhcPs) (LHsExpr GhcPs) -> Bool) ds, labelled (hfbLHS field), keySpan <- keysIn (hfbRHS field)],
      [keySpan | field <- map unLoc (listify (const True :: LHsRecUpdField GhcPs GhcPs -> Bool) ds), labelled (hfbLHS field), keySpan <- keysIn (hfbRHS field)]
    ]
  where
    labelled :: (Data a) => a -> Bool
    labelled lhs = any (isProtocolPairName . rdrText) (listify (const True :: RdrName -> Bool) lhs)

    keysIn :: LHsExpr GhcPs -> [SrcLoc.SrcSpan]
    keysIn value =
      [ getLocA key
      | tuple <- listify (const True :: LHsExpr GhcPs -> Bool) value,
        ExplicitTuple _ (Present _ key : _) _ <- [unLoc tuple]
      ]

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
    | operatorName op `elem` [Just "+", Just "-", Just "*"] ->
        case (isLiteral (peelExpr l), isLiteral (peelExpr r)) of
          (False, True) -> [getLocA r]
          (True, False) -> [getLocA l]
          _ -> []
  _ -> []

-- | A literal compared with something that is not one, as in @length xs > 1@.
--
-- That is a claim about the other value, the way an offset from it is, rather
-- than a value the test runs on.
comparisons :: LHsExpr GhcPs -> [SrcLoc.SrcSpan]
comparisons le = case unLoc le of
  OpApp _ l op r
    | operatorName op `elem` map Just [">", "<", ">=", "<=", "==", "/="] ->
        case (isLiteral (peelExpr l), isLiteral (peelExpr r)) of
          (False, True) -> [getLocA r]
          (True, False) -> [getLocA l]
          _ -> []
  _ -> []

-- | The expected side of an assertion written as a section, as in
-- @(`shouldBe` 1)@, the step of an increment written as one, as in @(+ 1)@,
-- and the zero a counter starts at.
sectionsAndCounters :: LHsExpr GhcPs -> [SrcLoc.SrcSpan]
sectionsAndCounters le = case unLoc le of
  SectionR _ op arg
    | maybe False (T.isPrefixOf "should") (operatorName op) -> [getLocA arg]
    | operatorName op `elem` [Just "+", Just "-", Just "*"] -> [getLocA arg]
  SectionL _ arg op
    | operatorName op `elem` [Just "+", Just "-", Just "*"] -> [getLocA arg]
  _ -> case namedApplicationOf le of
    Just (name, [start])
      | name `elem` ["newIORef", "newTVar", "newTVarIO", "newMVar", "newSTRef"] && isZero start -> [getLocA start]
    _ -> []

-- | Whether this is the literal zero, which is where a counter starts.
isZero :: LHsExpr GhcPs -> Bool
isZero e = case unLoc (peelExpr e) of
  HsOverLit _ OverLit {ol_val = HsIntegral il} -> il_value il == 0
  _ -> False

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
-- separator or a prefix describes the format a value is taken apart by. A
-- selector names a part of a page, and a form or query parameter's name is
-- fixed by the handler it is sent to.
--
-- The generator a property is quantified over is the replacement this rule
-- asks for. A QuickCheck setting or a retry delay is its first argument alone,
-- because what it wraps is a test or a policy like any other.
firstArgumentNames :: [Text]
firstArgumentNames =
  [ "expectationFailure",
    "error",
    "errorWithoutStackTrace",
    "userError",
    "fail",
    "throwString",
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
    "splitOn",
    "breakOn",
    "breakOnEnd",
    "count",
    "stripPrefix",
    "stripSuffix",
    "replace",
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
    "parseMaybe",
    "parseEither",
    "parseFail",
    "mkStatus",
    "tupleToHostAddress",
    "SockAddrInet",
    "setSocketOption",
    "listen",
    "subtract",
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
