module Hopinion.Check.Hs.NoFilePath.Extract (namesOf) where

import qualified Data.Text as T
import GHC.Data.FastString (unpackFS)
import GHC.Parser.Lexer (Token (..))
import Hopinion.Check.Hs.NoFilePath.Fact
import Hopinion.Extract.Ghc (declScopeOf)
import Hopinion.Facts.Module
import Hopinion.Parse (ParsedModule (..), SourceToken (..))

-- | Every capitalised name the module writes, which is every @conid@ token.
--
-- A qualified name is one token holding both halves, so a bare name here is one
-- the code wrote and not the tail of a module name.
namesOf :: ModuleContext -> ParsedModule -> [NameFact]
namesOf ctx parsed =
  [ NameFact
      { nameFactText = T.pack (unpackFS name),
        nameFactSpan = sp,
        nameFactScope = declScopeOf (moduleContextRef ctx) (moduleContextDecls ctx) sp
      }
  | SourceToken (ITconid name) sp <- parsedModuleTokens parsed
  ]
