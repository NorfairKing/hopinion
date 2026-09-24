{-# LANGUAGE OverloadedStrings #-}

-- | A comment is reported when it says two places move together /and/ names
-- one of them as code, which is two signals rather than one.
--
-- One signal on its own reports code nobody would change. Measured over a
-- repository of a thousand modules, the phrase alone reported nine comments and
-- six of them were wrong: four said @keep the hub's type in sync with its
-- forge@, where the keeping is what the program does at run time against an
-- external system rather than what the next person editing has to do, and two
-- argued that there is deliberately no second place. The name alone was worse,
-- reporting twenty-two comments and none worth acting on, because naming a
-- sibling definition to orient a reader is what good Haddock is made of.
--
-- Together they reported one comment, which was the coupling. So the two are
-- one rule and not two: neither half stands up alone.
module Hopinion.Check.Comment.CouplingConnection.Rule (rule) where

import Data.Char (isAlpha, isAlphaNum, isUpper)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Hopinion.Comment
import Hopinion.Facts.Module
import Hopinion.Facts.Place
import Hopinion.Rule
import Hopinion.Rule.Id

rule :: Rule
rule =
  Rule
    { ruleId = RuleId "CommentCouplingConnection",
      ruleText = "A comment that couples two named places carries a tag or a ref.",
      ruleWhy =
        "Prose saying that two places move together names neither of them to a\
        \ tool, so nothing brings a reader to the other one when this changes. A\
        \ tag and a ref do, and tagref and marginalia report the pair when one\
        \ half of it goes missing. Write [tag:Name] here and [ref:Name] there, or\
        \ [check:tag Name] and [check:ref Name].",
      ruleImpl = ModuleRule (FromSource check)
    }

check :: ModuleContext -> CheckResult
check mf =
  let connected = scopesWithAConnection mf
      scopeOf = scopeOfComment (moduleContextRef mf)
   in findingsResult
        [ Finding
            { findingRule = ruleId rule,
              findingScope = scopeOf cf,
              findingSpan = commentFactSpan cf,
              findingMessage =
                T.concat
                  [ "A coupling written as prose: \"",
                    phrase,
                    "\", in a sentence that names ",
                    name,
                    "."
                  ]
            }
        | cf <- moduleContextComments mf,
          commentFactStyle cf /= StylePragma,
          not (S.member (scopeOf cf) connected),
          Just (phrase, name) <- [couplingIn (commentFactText cf)]
        ]

-- | The scopes a connection was written in, which is coarser than the comment
-- it was written on.
--
-- A scope rather than a comment block, because the two halves are routinely
-- written apart: a Haddock line carrying the annotation and the prose under it
-- are two blocks, since a change of comment style ends one. They are one
-- declaration, which is also the granularity a suppression is matched at, so it
-- is the granularity a reader can predict.
scopesWithAConnection :: ModuleContext -> Set ScopeKey
scopesWithAConnection mf =
  S.fromList
    [ scopeOfComment (moduleContextRef mf) cf
    | cf <- moduleContextComments mf,
      hasConnection (commentFactText cf)
    ]

-- | Whether a comment carries a half of a tagref or a marginalia pair.
--
-- Only the paired forms count. A bare marginalia @[check]@ asks a reviewer to
-- look at this code and says nothing about any other code, which is the thing
-- that was missing.
hasConnection :: Text -> Bool
hasConnection = any isConnection . bracketed

-- | What each bracket in the text holds, without the brackets.
--
-- Both grammars announce themselves with a bracket and disagree on everything
-- inside it, so the inside is read once here and judged once below.
bracketed :: Text -> [Text]
bracketed = delimitedBy "[" "]"

-- | Every spelling of one half of a pair, each of which has to name the other
-- half: an annotation naming nothing connects nothing.
isConnection :: Text -> Bool
isConnection inside =
  let named :: Text -> Bool
      named m = maybe False (not . T.null . T.strip) (T.stripPrefix m (T.stripStart inside))
   in any named ["tag:", "ref:", "check:tag ", "check:ref "]

-- | The first coupling a comment states, and the first name marked in the
-- sentence that states it. A comment saying it twice is still one coupling.
--
-- The name is reported as one the sentence names rather than as one of the two
-- places coupled, because it is the first marked name and nothing here can tell
-- which of several a coupling is between: @unlike \'Foo\', keep \'Bar\' and
-- \'Baz\' in sync@ names three and couples two of them.
--
-- Read a sentence at a time, because that is the unit a reader means one thing
-- in: a comment that says @keep@ in one sentence and names a type three
-- sentences later has not said the two are connected.
--
-- Whitespace is normalised first, since a comment block is several lines joined
-- and a phrase falls across the join as often as not. Case is folded per
-- sentence rather than before the split, because 'markedNames' still has to see
-- which words were capitalised.
couplingIn :: Text -> Maybe (Text, Text)
couplingIn t =
  let couplingsIn :: Text -> [(Text, Text)]
      couplingsIn sentence =
        let folded = T.toLower sentence
            phrases =
              filter (`T.isInfixOf` folded) couplingPhrases
                ++ (if asksToKeep folded then filter (`T.isInfixOf` folded) alikePhrases else [])
         in [(phrase, name) | name <- take 1 (markedNames sentence), phrase <- take 1 phrases]
   in case concatMap couplingsIn (sentencesOf (T.unwords (T.words t))) of
        (found : _) -> Just found
        [] -> Nothing

-- | Sentences, taken at every mark that ends a clause. An abbreviation splits
-- one sentence into two, which costs a finding and never invents one.
sentencesOf :: Text -> [Text]
sentencesOf = T.split (`elem` (".!?;:" :: String))

-- | Whether a sentence asks for something to be held the way it is, which is
-- what separates an instruction to the next person editing this code from a
-- description of what the code does while it runs.
asksToKeep :: Text -> Bool
asksToKeep s = any ((`elem` keepingWords) . T.filter isAlpha) (T.words s)

keepingWords :: [Text]
keepingWords =
  [ "keep",
    "keeps",
    "kept",
    "keeping",
    "stay",
    "stays",
    "stayed",
    "staying",
    "remain",
    "remains"
  ]

-- | Prose that says this code and other code are edited together, which needs
-- nothing else in the sentence to mean that.
couplingPhrases :: [Text]
couplingPhrases =
  [ "change together",
    "changed together",
    "changes together",
    "update together",
    "updated together",
    "updates together"
  ]

-- | Prose that says two things are alike, which is a coupling only where the
-- sentence also asks for them to be held that way. On its own it is as likely
-- to be describing what the code does at run time. A replica that is already
-- alike is a state somebody waits for. A replica somebody is told to hold that
-- way is an instruction to whoever edits this next.
--
-- Both lists are short on purpose, and every entry earns its place by having no
-- reading that is not a coupling. The cost of a wrong entry is a finding whose
-- only honest answer is a suppression, and a rule people suppress is a rule they
-- stop reading. So a phrase that merely compares two things, like "the same way"
-- or "must match", is in neither list: that is how most prose describes
-- anything.
alikePhrases :: [Text]
alikePhrases =
  [ "in sync",
    "in lockstep",
    "in tandem"
  ]

-- | The names a sentence marks as code rather than as prose: Haddock's
-- @\'Name\'@, and the @\`Name\`@ and @\@Name\@@ spellings people write beside
-- it.
--
-- Marked rather than guessed. A word bag over prose would report every English
-- word that is also a constructor, and a person who marked a name meant a
-- reference to code by it. Every finding this rule made over a thousand modules
-- came from the Haddock spelling; the other two are here because they cost
-- nothing and a repository that writes them means the same thing.
--
-- Capitalised only, which is what a module's tokens record. A lower-case name
-- in a comment cannot be told from the same word in a sentence without
-- occurrences that extraction does not produce, so asking for one would be
-- asking a question with no answer.
markedNames :: Text -> [Text]
markedNames t =
  [ name
  | (opening, closing) <- [("'", "'"), ("`", "`"), ("@", "@")],
    name <- delimitedBy opening closing t,
    startsCapitalised name,
    T.all isNameChar name
  ]

startsCapitalised :: Text -> Bool
startsCapitalised name = case T.uncons name of
  Just (c, _) -> isUpper c
  Nothing -> False

-- | A qualified name is one name, so the separator is part of it.
isNameChar :: Char -> Bool
isNameChar c = isAlphaNum c || c == '.' || c == '_'

-- | What each pair of delimiters encloses, left to right and without nesting,
-- which is all either spelling ever has.
delimitedBy :: Text -> Text -> Text -> [Text]
delimitedBy opening closing t = case T.breakOn opening t of
  (_, fromOpen)
    | T.null fromOpen -> []
    | otherwise ->
        let afterOpen = T.drop (T.length opening) fromOpen
         in case T.breakOn closing afterOpen of
              (_, fromClose) | T.null fromClose -> []
              (inside, fromClose) ->
                inside : delimitedBy opening closing (T.drop (T.length closing) fromClose)
