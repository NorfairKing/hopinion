-- | The rules this tool ships with. Adding one is a line here and nothing else.
--
-- A list rather than a set, because building a set can fail and this list can
-- only fail it on something a test asserts rather than something a run could
-- hit, so a value carrying that failure would hand every caller a branch it
-- cannot act on.
module Hopinion.Rule.Registry (builtinRules) where

import qualified Hopinion.Check.Comment.BareTodo.Rule as BareTodo
import qualified Hopinion.Check.Hs.NoCustomShowRead.Rule as NoCustomShowRead
import qualified Hopinion.Check.Hs.NoFilePath.Rule as NoFilePath
import qualified Hopinion.Check.Hs.NoSemigroupOnText.Rule as NoSemigroupOnText
import qualified Hopinion.Check.Package.GenValidInGenPackage.Rule as GenValidInGenPackage
import qualified Hopinion.Check.Project.GenValidSpecPerGenValid.Rule as GenValidSpecPerGenValid
import Hopinion.Rule (Rule)

builtinRules :: [Rule]
builtinRules =
  [ BareTodo.rule,
    GenValidInGenPackage.rule,
    GenValidSpecPerGenValid.rule,
    NoCustomShowRead.rule,
    NoFilePath.rule,
    NoSemigroupOnText.rule
  ]
