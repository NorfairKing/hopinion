{-# LANGUAGE ScopedTypeVariables #-}

-- | Beside the rule rather than in the shared extraction, because nothing else
-- asks how a record value is laid out. What is shared is how an expression is
-- taken apart, and that comes from 'Hopinion.Extract.Ghc'.
module Hopinion.Check.Hs.RecordFieldPerLine.Extract (crowdedRecordsOf) where

import Data.Generics (listify)
import Data.List (nub)
import GHC.Hs
import GHC.Types.SrcLoc (GenLocated, unLoc)
import Hopinion.Check.Hs.RecordFieldPerLine.Fact
import Hopinion.Extract.Ghc
import Hopinion.Facts.Decl
import Hopinion.Facts.Place
import Path (File, Path, Rel)

-- | Every record value in the module that writes two of its fields on one
-- line, wherever it is written.
--
-- Over the expressions rather than over the declarations, because a record
-- value in a where clause, in a list or in an argument is the same node as one
-- on the right of a top-level binding.
--
-- Record patterns are not record values and are left alone: a pattern names
-- what it takes apart rather than what it builds, and nothing about a diff of
-- one argues for the layout this rule asks of the other.
crowdedRecordsOf :: Path Rel File -> ModuleRef -> [DeclFact] -> [LHsDecl GhcPs] -> [CrowdedRecord]
crowdedRecordsOf rp ref decls ds =
  [ CrowdedRecord
      { crowdedRecordUse = use,
        crowdedRecordFields = fromIntegral (length fieldLines),
        crowdedRecordSpan = sp,
        crowdedRecordScope = declScopeOf ref decls sp
      }
  | le <- listify (const True :: LHsExpr GhcPs -> Bool) ds,
    Just (use, fieldLines) <- [layoutOf rp le],
    length fieldLines > 1,
    length (nub fieldLines) < length fieldLines,
    let sp = spanOfSrcSpan rp (getLocA le)
  ]

-- | What this expression is and the line each of its fields starts on, or
-- nothing when it is not a record value.
--
-- Start lines rather than the expression's own span, because a field whose
-- value runs onto a second line makes the expression multi-line without
-- putting any two fields on a line together, and it is the fields this rule is
-- about.
layoutOf :: Path Rel File -> LHsExpr GhcPs -> Maybe (RecordUse, [Word])
layoutOf rp le = case unLoc le of
  RecordCon {rcon_flds = flds} ->
    Just (RecordConstructed, map (startLine rp) (rec_flds flds) ++ dotdotLines rp flds)
  RecordUpd {rupd_flds = flds} ->
    Just
      ( RecordUpdated,
        case flds of
          RegularRecUpdFields {recUpdFields = fs} -> map (startLine rp) fs
          OverloadedRecUpdFields {olRecUpdFields = fs} -> map (startLine rp) fs
      )
  _ -> Nothing

-- | The @..@ of a wildcard, which stands where a field would and so takes up a
-- line the same way one does.
dotdotLines :: Path Rel File -> HsRecFields GhcPs (LHsExpr GhcPs) -> [Word]
dotdotLines rp flds = case rec_dotdot flds of
  Nothing -> []
  Just dd -> [startLine rp dd]

startLine :: (HasLoc a) => Path Rel File -> GenLocated a e -> Word
startLine rp = positionLine . spanStart . spanOfSrcSpan rp . getLocA
