{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Beside the rule rather than in the shared extraction, because nothing else
-- asks what a test body is. What is shared is how an expression is taken apart,
-- and that comes from 'Hopinion.Extract.Ghc'.
module Hopinion.Check.Test.NoDummyValues.Extract (testLiteralsOf) where

import Data.Foldable (toList)
import Data.Generics (listify)
import Data.List (genericLength, nub, sortBy)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Set as S
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
import Hopinion.Facts.Name (DeclName (..))
import Hopinion.Facts.Place
import Path (File, Path, Rel)

-- | Every literal a test may run on, and what receives each one.
--
-- That is a literal in the body of a test, which is the argument after the
-- description, or in any binding of this module the body names, followed for
-- as long as there are names to follow. Where a value is bound does not change
-- whether it was made up, so moving one out of the test does not hide it.
-- A literal in the description is the test's name, and one that no test reaches
-- is not a value any test runs on.
--
-- Whether a literal is made up depends on what receives it, which is a question
-- about where that code is defined, so it is left to the rule with the build in
-- hand. What is decided here is what the source alone settles: a literal a
-- table maps a constructor to is a specification of that constructor.
--
-- Names are followed by spelling rather than by what they resolve to, because
-- resolving them is renaming. A local that shadows another name drags that
-- name's binding in as well, which errs towards reporting.
--
-- Nothing outside a test file, because nothing else asks and this is the most
-- expensive reading of a module there is.
testLiteralsOf :: Path Rel File -> ModuleRef -> [DeclFact] -> [LHsDecl GhcPs] -> TestLiterals
testLiteralsOf rp ref decls ds
  | not (isSpecFilePath rp) =
      TestLiterals
        { testLiteralsLiterals = [],
          testLiteralsUses = [],
          testLiteralsParameters = [],
          testLiteralsArities = []
        }
  | otherwise =
      TestLiterals
        { testLiteralsLiterals =
            [ TestLiteral
                { testLiteralText = T.pack (showPprUnsafe (unLoc l)),
                  testLiteralSpan = sp,
                  testLiteralScope = declScopeOf ref decls sp,
                  testLiteralOwner = spanOfSrcSpan rp owner,
                  testLiteralConsumers = consumersAt loc
                }
            | l <- listify isLiteral ds,
              let loc = getLocA l,
              within reached loc,
              not (within exempt loc),
              let sp = spanOfSrcSpan rp loc,
              Just owner <- [ownerOf loc]
            ],
          testLiteralsUses =
            [ BindingUse
                { bindingUseName = name,
                  bindingUseConsumers = consumersAt loc
                }
            | (name, loc) <- occurrences,
              name `S.member` boundNames,
              not (within exempt loc),
              within reached loc
            ],
          testLiteralsParameters =
            [ ParameterUse
                { parameterUseFunction = rdrText (unLoc name),
                  parameterUseIndex = index,
                  parameterUseConsumers = consumersAt loc
                }
            | lb <- listify (const True :: LHsBind GhcPs -> Bool) ds,
              FunBind {fun_id = name, fun_matches = mg} <- [unLoc lb],
              m <- map unLoc (unLoc (mg_alts mg)),
              (index, p) <- zip [0 ..] (unLoc (m_pats m)),
              parameter <- patternNames p,
              (used, loc) <- occurrences,
              used == parameter,
              loc `SrcLoc.isSubspanOf` getLocA lb,
              not (within exempt loc)
            ],
          testLiteralsArities =
            [ FunctionArity
                { functionArityName = rdrText (unLoc name),
                  functionArityParameters = foldl' max 0 (map (genericLength . unLoc . m_pats . unLoc) (unLoc (mg_alts mg)))
                }
            | lb <- listify (const True :: LHsBind GhcPs -> Bool) ds,
              FunBind {fun_id = name, fun_matches = mg} <- [unLoc lb]
            ]
        }
  where
    expressions :: [LHsExpr GhcPs]
    expressions = listify (const True :: LHsExpr GhcPs -> Bool) ds

    calls :: [Call]
    calls = nub (concatMap callsAt expressions ++ recordCalls ds)

    bodies :: [SrcLoc.SrcSpan]
    -- One body per test, the whole of it. The parser nests an infix spine to
    -- the left, so a test written with a $ is also seen once per operator
    -- after it, each time with only part of its body.
    bodies =
      M.elems
        ( M.fromListWith
            (\a b -> if a `SrcLoc.isSubspanOf` b then b else a)
            [ (spanOfSrcSpan rp headLoc, body)
            | Call {callHead = (name, headLoc), callArguments = _ : (body, _) : _} <- calls,
              isTestName name
            ]
        )

    bindings :: [(Text, SrcLoc.SrcSpan)]
    bindings = bindingsOf ds

    boundNames :: S.Set Text
    boundNames = S.fromList (map fst bindings)

    definedHere :: S.Set Text
    definedHere =
      S.unions
        [ boundNames,
          S.fromList (map (unDeclName . declFactName) decls),
          S.fromList [rdrText (unLoc n) | ConDeclH98 {con_name = n} <- listify (const True :: ConDecl GhcPs -> Bool) ds],
          S.fromList [rdrText (unLoc n) | ConDeclGADT {con_names = ns} <- listify (const True :: ConDecl GhcPs -> Bool) ds, n <- toList ns]
        ]

    occurrences :: [(Text, SrcLoc.SrcSpan)]
    occurrences = [(rdrText (unLoc n), getLocA e) | e <- expressions, HsVar _ n <- [unLoc e]]

    reached :: [SrcLoc.SrcSpan]
    reached = reachedFrom bindings occurrences (bodies ++ testHelperArguments)

    -- What a function of this file that writes tests is handed, as in
    -- @parsesAs "release/*" ...@ at the top of a spec: those values reach the
    -- tests it writes, by its parameters.
    testHelperArguments :: [SrcLoc.SrcSpan]
    testHelperArguments =
      [ arg
      | Call {callHead = (name, _), callArguments = args} <- calls,
        name `S.member` testWriters,
        (arg, _) <- args
      ]

    testWriters :: S.Set Text
    testWriters =
      S.fromList
        [ functionName
        | (functionName, s) <- bindings,
          any (`SrcLoc.isSubspanOf` s) bodies
        ]

    tables :: [SrcLoc.SrcSpan]
    tables = constructorTables ds

    -- What the source alone settles, whoever receives it: a table from
    -- constructors, an offset from a value that is not a literal, and how a
    -- property generates its values.
    exempt :: [SrcLoc.SrcSpan]
    exempt = tables ++ concatMap offsetsAt expressions ++ generatorArguments calls

    blocks :: [SrcLoc.SrcSpan]
    blocks = [getLocA e | e <- expressions, isBlock (unLoc e)]

    -- The innermost test a literal is in, and the outermost binding it was
    -- reached through when it is in none, so that a binding with lets or a
    -- comprehension inside it is still one thing to answer for.
    ownerOf :: SrcLoc.SrcSpan -> Maybe SrcLoc.SrcSpan
    ownerOf loc = case innermost [b | b <- bodies, loc `SrcLoc.isSubspanOf` b] of
      Just body -> Just body
      Nothing -> outermost [r | r <- reached, loc `SrcLoc.isSubspanOf` r]

    -- What receives a value written here, innermost first, up to the test or
    -- the binding it is written in, or to the lambda or @do@ block it is
    -- written in if that is nearer. A binding ends the list with its own name,
    -- because what receives the value from there is wherever that name is used.
    --
    -- A lambda or a @do@ block stops it because what is written in one is not
    -- what that block is handed to: a literal in a step of a @do@ block passed
    -- to a helper is given to that step, not to the helper.
    consumersAt :: SrcLoc.SrcSpan -> [Consumer]
    consumersAt loc =
      let region = innermost [r | r <- bodies ++ map snd bindings, loc `SrcLoc.isSubspanOf` r]
          boundary = innermost [b | b <- blocks, loc `SrcLoc.isSubspanOf` b, maybe True (b `SrcLoc.isSubspanOf`) region]
          -- Strictly inside the block: an argument that is the block itself is what
          -- the block is handed to, which is beyond it.
          inRegion arg = maybe True (arg `SrcLoc.isSubspanOf`) region && maybe True (\b -> arg `SrcLoc.isSubspanOf` b && arg /= b) boundary
          enclosing =
            sortBy
              innerFirst
              [ (arg, (index, call))
              | call <- calls,
                (index, (arg, _)) <- zip [0 ..] (callArguments call),
                loc `SrcLoc.isSubspanOf` arg,
                inRegion arg
              ]
          callConsumers =
            nub
              [ ConsumerCall (nameAt (callHead call)) index
              | (_, (index, call)) <- enclosing
              ]
          bindingConsumer =
            [ ConsumerBinding name
            | Nothing <- [boundary],
              Just r <- [region],
              (name, s) <- bindings,
              s == r
            ]
       in callConsumers ++ take 1 bindingConsumer

    nameAt :: (Text, SrcLoc.SrcSpan) -> NameAt
    nameAt (name, loc) =
      NameAt
        { nameAtText = name,
          nameAtPosition = spanStart (spanOfSrcSpan rp loc),
          nameAtDefinition = if name `S.member` definedHere then DefinitionHere else DefinitionElsewhere
        }

-- | One place a name is applied, and its arguments: where each is written, and
-- the name it is when it is a bare name.
data Call = Call
  { callHead :: !(Text, SrcLoc.SrcSpan),
    callArguments :: ![(SrcLoc.SrcSpan, Maybe (Text, SrcLoc.SrcSpan))]
  }
  deriving (Eq)

-- | The smaller of two regions that both contain one point first, which is the
-- one written closer to it.
innerFirst :: (SrcLoc.SrcSpan, a) -> (SrcLoc.SrcSpan, a) -> Ordering
innerFirst (a, _) (b, _)
  | a == b = EQ
  | a `SrcLoc.isSubspanOf` b = LT
  | b `SrcLoc.isSubspanOf` a = GT
  | otherwise = EQ

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
-- Terminates because a region is only added once and there are finitely many
-- bindings.
reachedFrom :: [(Text, SrcLoc.SrcSpan)] -> [(Text, SrcLoc.SrcSpan)] -> [SrcLoc.SrcSpan] -> [SrcLoc.SrcSpan]
reachedFrom bindings occurrences = go
  where
    go :: [SrcLoc.SrcSpan] -> [SrcLoc.SrcSpan]
    go regions =
      let named = [n | (n, loc) <- occurrences, within regions loc]
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

-- | A lambda or a @do@ block, which is where what a value is handed to stops
-- being what it is written in.
isBlock :: HsExpr GhcPs -> Bool
isBlock = \case
  HsLam {} -> True
  HsDo {} -> True
  _ -> False

isLiteral :: LHsExpr GhcPs -> Bool
isLiteral le = case unLoc le of
  HsLit {} -> True
  HsOverLit {} -> True
  _ -> False

-- | The sydtest functions that take a description and then a test, which is
-- what a test is.
isTestName :: Text -> Bool
isTestName name =
  fromMaybe name (T.stripPrefix "x" name)
    `elem` ( "prop"
               : [ T.pack (concat [base, variant])
                 | base <- ["it", "specify"],
                   variant <- ["", "WithOuter", "WithBoth", "WithAll"]
                 ]
           )

-- | A literal added to, taken from, multiplied with or compared with something
-- that is not a literal, as in @index + 1@ or @length xs > 1@, or written as
-- one side of such a section, as in @(+ 1)@, or where an open-ended count
-- starts, as in @zip [0 ..] xs@.
--
-- That states where a value is relative to another, which is a claim about the
-- other value rather than a value made up in its place.
offsetsAt :: LHsExpr GhcPs -> [SrcLoc.SrcSpan]
offsetsAt le = case unLoc le of
  ArithSeq _ _ (From start) -> [getLocA start]
  OpApp _ l op r
    | isRelation op -> case (isLiteral (peelExpr l), isLiteral (peelExpr r)) of
        (False, True) -> [getLocA r]
        (True, False) -> [getLocA l]
        _ -> []
  SectionL _ arg op | isRelation op -> [getLocA arg]
  SectionR _ op arg | isRelation op -> [getLocA arg]
  _ -> []
  where
    isRelation :: LHsExpr GhcPs -> Bool
    isRelation op = case unLoc op of
      HsVar _ n -> rdrText (unLoc n) `elem` ["+", "-", "*", ">", "<", ">=", "<=", "==", "/="]
      _ -> False

-- | Every argument of a @forAll@ function but the property, which is how the
-- values the property runs on are generated rather than values of their own.
generatorArguments :: [Call] -> [SrcLoc.SrcSpan]
generatorArguments calls =
  [ arg
  | Call {callHead = (name, _), callArguments = args} <- calls,
    T.isPrefixOf "forAll" name,
    (arg, _) <- take (length args - 1) args
  ]

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

-- | Every named function or constructor applied at this node, by juxtaposition,
-- with a @$@ before its last argument, or as an infix operator.
--
-- The parser leaves fixity unresolved, so the last two are read off the infix
-- spine, which is in source order. Everything after a @$@ is its argument,
-- because nothing binds looser; an operator\'s other operands are the ones
-- written beside it.
callsAt :: LHsExpr GhcPs -> [Call]
callsAt le =
  let (leftmost, rest) = spineOf le
   in maybe [] pure (prefixCall le) ++ infixCalls (leftmost : map snd rest) (map fst rest)

infixCalls :: [LHsExpr GhcPs] -> [LHsExpr GhcPs] -> [Call]
infixCalls operands ops = case (operands, ops) of
  (x : after@(y : ys), op : ops') ->
    let here = case unLoc op of
          HsVar _ n
            | rdrText (unLoc n) == "$" ->
                [ call {callArguments = callArguments call ++ [(foldl' SrcLoc.combineSrcSpans (getLocA y) (map getLocA ys), Nothing)]}
                | Just call <- [prefixCall x]
                ]
            | otherwise ->
                [ Call
                    { callHead = (rdrText (unLoc n), getLocA n),
                      callArguments = [argument x, argument y]
                    }
                ]
          _ -> []
     in here ++ infixCalls after ops'
  _ -> []

-- | A name applied to arguments by juxtaposition.
prefixCall :: LHsExpr GhcPs -> Maybe Call
prefixCall = go []
  where
    go :: [LHsExpr GhcPs] -> LHsExpr GhcPs -> Maybe Call
    go acc le = case unLoc (peelExpr le) of
      HsApp _ f x -> go (x : acc) f
      HsAppType _ f _ -> go acc f
      HsVar _ n ->
        Just
          Call
            { callHead = (rdrText (unLoc n), getLocA n),
              callArguments = map argument acc
            }
      _ -> Nothing

-- | Where an argument is written, and the name it is when it is a bare name.
argument :: LHsExpr GhcPs -> (SrcLoc.SrcSpan, Maybe (Text, SrcLoc.SrcSpan))
argument arg =
  ( getLocA arg,
    case unLoc (peelExpr arg) of
      HsVar _ n -> Just (rdrText (unLoc n), getLocA n)
      _ -> Nothing
  )

-- | A record built, as a call: its constructor applied to every field it is
-- given.
--
-- A field set in an update is not a call, any more than a constructor is one
-- that decides anything: setting a field builds a value and runs no code.
recordCalls :: [LHsDecl GhcPs] -> [Call]
recordCalls ds =
  [ Call
      { callHead = (rdrText (unLoc con), getLocA con),
        callArguments = [argument (hfbRHS (unLoc field)) | field <- fields]
      }
  | RecordCon {rcon_con = con, rcon_flds = HsRecFields {rec_flds = fields}} <- listify isRecordCon ds
  ]
  where
    isRecordCon :: HsExpr GhcPs -> Bool
    isRecordCon = \case
      RecordCon {} -> True
      _ -> False
