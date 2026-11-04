module Hopinion.Check.Project.SydtestOnly.Extract (importsOf) where

import qualified Data.Text as T
import GHC.Hs
import GHC.Types.SrcLoc (unLoc)
import Hopinion.Check.Project.SydtestOnly.Fact
import Hopinion.Extract.Ghc
import Hopinion.Facts.Module
import Hopinion.Facts.Name (ModuleKey (..))
import Hopinion.Parse (ParsedModule (..))

-- | Every import, spanning the module name rather than the whole import, so
-- that an import list running over several lines underlines what is wrong
-- with it.
importsOf :: ModuleContext -> ParsedModule -> [ImportFact]
importsOf ctx parsed =
  [ ImportFact
      { importFactModule = ModuleKey (T.pack (moduleNameString (unLoc name))),
        importFactSpan = spanOfSrcSpan (moduleContextPath ctx) (getLocA name)
      }
  | limport <- hsmodImports (unLoc (parsedModuleAst parsed)),
    let name = ideclName (unLoc limport)
  ]
