# Rules

Every rule this tool could enforce that it does not enforce yet, sorted by how
it can be enforced. A rule that ships leaves this file: what is here is work.

The five that ship are `CommentBareTodo`, `HsNoCustomShowRead`, `HsNoFilePath`,
`HsGenValidInGenPackage` and `TestGenValidSpecPerGenValid`.

## How to read this

**Bucket**, what kind of enforcement the rule admits:

| | |
|---|---|
| `A` | Already enforced by an existing tool. Do not reimplement. |
| `B` | Mechanizable as a direct check. |
| `C` | Not decidable as stated, but enforceable as an obligation: required code must exist, or the site that creates the requirement carries an annotation saying why not. |
| `D` | Irreducibly human judgement. Better as a review checklist than as a check. |

**Tier**, how much information the check needs, in increasing order of cost:

| | |
|---|---|
| `layout` | File tree, cabal / package.yaml, config files. No Haskell parsing. |
| `config` | Contents of `.hlint.yaml`, `weeder.toml`, `.pre-commit-config.yaml`. |
| `syntax` | `ghc-lib-parser` AST, one module at a time. |
| `comment` | AST plus attached comments. |
| `project` | Union of per-module facts across the repository. |
| `types` | Needs `.hie` type information. |
| `nix` | Nix files. Deliberately out of scope for now. |

**Class**, the confidence gate a check must clear before it ships. Every rule is
on by default and every finding fails the run, so this is not a runtime setting.
It says how much suppression traffic the check is expected to generate, which
decides whether it is ready to ship and in what order.

| | |
|---|---|
| `error` | Expected to reach zero false positives. Ship freely. |
| `warn` | Needs judgement, so expect a standing population of suppressions. Ships only once the false positive rate makes suppressing the residue honest. |
| `ratchet` | High finding volume at adoption. Ships suppressed at every site, with the population counted down. |

Rule ids are stable. They are what a check module cites and what an
`[allow:...]` annotation names.

## Comments
| ID | Rule | Bucket | Tier | Class | Note |
|---|---|---|---|---|---|
| `CommentDefaultNone` | Default to no comment | D | | | The framing for everything below, not itself a check |
| `CommentNameInsteadOfComment` | Ask whether a better name or extraction removes the need | D | | | The judgement call a checker cannot make |
| `CommentTruth` | Keep every comment true | D | | | Undecidable in general. Approached by `CommentRestatedLiteral` and `CommentDanglingName` |
| `CommentRefactorSafe` | A comment must survive rename, reorder, extract, inline, move | B | | | Parent rule. Implemented as the five subchecks below |
| `CommentLengthProportional` | As long as its subject was hard to figure out | D | | | |
| `CommentWhyNotWhat` | Record why, not what | D | | | Stated in more than one guide |
| `CommentHazard` | Hazards not visible from the code in front of you | D | | | Prescribes a comment, cannot be checked |
| `CommentWorkaroundUpstreamReason` | A workaround must carry the upstream reason | B | comment | warn | Comment matching workaround / hack / bug in must contain a URL or issue ref |
| `CommentCorrectnessArgument` | Subtle races, orderings, timing windows earn a comment | D | | | |
| `CommentAnnotationsLoadBearing` | `[check]`, `[check:tag]`, `[check:ref]`, tagref | A | | | marginalia and tagref own their own grammars |
| `CommentHaddockWhenUnclear` | Haddock when name and type do not carry the contract | D | | | Positive direction is not checkable |
| `CommentRestatesCode` | No restating the code | B | comment | warn | Word bag of the comment against split identifiers of the annotated decl |
| `CommentTestNarration` | No obvious test narration (`-- Run the query`) | B | comment | warn | Phrase list plus do-statement position, scoped to `*Spec.hs`. Higher confidence than the general case |
| `CommentDanglingName` | No mentioning a variable or function name | B | comment | error | Comment names an identifier the annotated decl does not reference. Module-local: needs no name table, so open imports do not weaken it |
| `CommentDanglingHaddockRef` | A Haddock `'name'` reference must resolve | B | project | error | Not stated in any guide: found by asking what a corpus of Haddock references makes checkable. A guard rather than a cleanup. Implements `CommentTruth` |
| `CommentArgumentPosition` | No mentioning an argument position | B | comment | error | Phrases like first argument, second arg, third parameter |
| `CommentRestatedLiteral` | No restating a literal value | B | comment | error | Literal in the comment also appears in the annotated code |
| `CommentPositionalReference` | No location references (`-- see the loop below`) | B | comment | error | below / above / following / preceding / line N |
| `CommentHaddockRestatesConstant` | Haddock states the policy, not the constant's value | B | comment | warn | `CommentRestatedLiteral` scoped to Haddock on a constant |
| `CommentHaddockListsCallers` | Haddock names callees, not callers or return shape | B | comment | warn | `CommentDanglingName` scoped to Haddock. Module-local for the same reason |
| `CommentSectionLabelInFunction` | No section labels inside a function | B | comment | warn | Comment at do-statement position. Suggest extraction |
| `CommentDuplicatesSignature` | No duplicating a type signature or a name | B | comment | warn | Comment word bag is a subset of the name and type words |
| `CommentCopyPastedTwin` | No comment block copy-pasted next to its twin | B | project | warn | Identical normalised comment blocks |
| `CommentChangelogNarrative` | No changelog or narrative (`-- Previously we used X`) | B | comment | error | Phrase list |
| `CommentCommentedOutCode` | No commented-out code | B | comment | error | Comment body parses as a Haskell decl or expression. Escape hatch for a labelled inactive path |
| `CommentUncheckedClaim` | No reassuring-but-unchecked claims | B | comment | warn | Phrase list: this is safe, cannot fail, never happens, guaranteed |
| `CommentDecorativeBanner` | No decorative separators or banners | B | comment | error | |
| `CommentOneSentencePerLine` | One sentence per line | B | comment | error | Needs an abbreviation exclusion list |
| `CommentWrap80` | Wrap at 80 columns where reasonable | B | comment | warn | Comments only. The rule already hedges |

## Prose

| ID | Rule | Bucket | Tier | Class | Note |
|---|---|---|---|---|---|
| `ProseNoVerifyWord` | Do not say verify unless it is formal verification | B | comment | error | Say try out, test, or assert |
| `ProseNoEmdash` | Avoid emdashes | B | comment | error | |
| `ProseNoNotXButY` | Avoid `It's not X, it's Y` phrasings | B | comment | warn | Phrase pattern |

## Haskell style

| ID | Rule | Bucket | Tier | Class | Note |
|---|---|---|---|---|---|
| `HsNoPartialFunctions` | Avoid partial functions | A | | | hlint, plus GHC `-Wincomplete-patterns` |
| `HsHlintBansPartial` | Ban them by default, using hlint | B | config | error | Meta-check: the ban rules are actually present in `.hlint.yaml` |
| `HsNoUnusedInstances` | Remove every instance you do not use | A | | | weeder |
| `HsNoWeederExceptions` | Avoid adding exceptions to `weeder.toml` | B | config | error | Every entry in `weeder.toml` must carry an adjacent annotation giving its reason |
| `HsFunctionBeforeInstance` | Before writing an instance, write a function | D | | | Operationalised by `HsClassesOnlyForPolymorphism` |
| `HsClassesOnlyForPolymorphism` | Only define type-classes for generic polymorphic code | B | project | warn | Class defined in the project with a single instance and no constrained-polymorphic use site |
| `HsInstancesLawAbiding` | Every instance must be law-abiding | C | project | error | Obligation: a law test exists for each instance of a law-bearing class |
| `HsRecordFieldPrefix` | Prefix naming for record fields | B | syntax | error | Field name starts with the lowercased type or constructor name |
| `HsNoLenses` | Do not use lenses if you can help it | B | layout | warn | lens in build-depends, or a `Control.Lens` import |
| `HsFewExtensions` | Use few language extensions | B | config | error | Extension outside the project allowlist. The allowlist is a config parameter; a one-off extension is annotated at its pragma |
| `HsPreferText` | Prefer `Text`; `String` for errors and interop only | C | types | ratchet | Per-occurrence, annotated at the `String` it concerns. High volume at adoption |
| `HsNoSemigroupOnText` | Do not use `<>` to concatenate strings or text | B | types | error | Needs the instantiated type at the use site. hlint structurally cannot do this |
| `HsTextViaPack` | Build `Text` with `unwords` on `String`s, then pack | B | syntax | warn | Flag `Data.Text.unwords` / `Data.Text.unlines` uses |
| `HsStrictFields` | Strict fields by default; document any lazy one | B | comment | error | Lazy field with no attached comment. Nice interplay with the comment tier |
| `HsExplicitImports` | Explicit import lists or qualified imports | A | | | GHC `-Wmissing-import-lists`, hlint |
| `HsMonomorphicOverConstraints` | Monomorphic functions over type-class constraints | B | types | warn | Constraint instantiated at exactly one type across all call sites |
| `HsPolymorphicWithoutConstraints` | Polymorphic functions if you do not need constraints | A | | | GHC `-Wredundant-constraints` covers the unused case |
| `HsLambdaCase` | `LambdaCase` over naming an argument to case on it | B | syntax | error | Both `f x = case x of` and the multi-equation form. Check first whether hlint already covers these two: if it does, the rule is bucket `A` |
| `HsMultilineRecord` | Multi-line record values with multiple fields | B | syntax | error | ormolu preserves the author's choice, so this is unenforced today |
| `HsLetOverWhere` | Prefer let-bindings over where bindings | B | syntax | warn | Exceptions for guards and multi-equation functions |
| `HsWhereHoldingLogic` | A where-bound helper with real logic becomes top-level | B | syntax | warn | Size and control-flow heuristic |
| `HsOneLetPerBinding` | One let-binding per variable bound, in do-blocks | B | syntax | error | Stated reason (accidental mutual recursion) is a correctness argument |
| `HsOrderEntrypointFirst` | Order entrypoint to implementation detail | B | syntax | warn | Module-local call graph. Flag a callee defined above its first caller |
| `HsTypesAboveUses` | Define types above the functions that use them | B | syntax | error | |
| `HsInstanceAdjacentToType` | Instances immediately after the type they are for | B | syntax | error | Subsumes the Validity-close-to-the-type rule |
| `HsInstancePriorityOrder` | `Validity` first, `NFData` next, the rest after | B | syntax | error | Config-driven priority list |
| `HsTestOneSpecPerFile` | One `spec :: Spec` per file, only `spec` exported | B | syntax | error | |
| `HsTestSpecTopmost` | `spec` is the top-most function in a test file | B | syntax | error | |
| `HsTestOrderGranularFirst` | Order tests granular to less granular | D | | | |
| `HsTestDescribeOrder` | `describe` groups follow definition-module order | B | project | warn | Resolve `describe` string literals against the source module's decl order |
| `HsLocalTypeSignatures` | Type signatures on let- and where-bound functions | B | syntax | warn | Only where the types are nameable |
| `HsIllegalStatesUnrepresentable` | Make illegal states unrepresentable | D | | | The parent principle |
| `HsNewtypeNotSynonym` | `newtype`s, not `type` synonyms, for domain scalars | B | syntax | error | `type X = Text / Int / String / Double` |
| `HsNoDomainBool` | Named two-constructor type over `Bool` for domain flags | C | syntax | ratchet | Scope to record fields and non-final argument positions. A predicate returning `Bool` is fine |
| `HsNoCatchAllOwnSum` | No catch-all `_ ->` on your own sum types | B | types | error | Needs to know the scrutinee type is project-defined |
| `HsNonempty` | `NonEmpty` for collections that must not be empty | D | | | |
| `HsUndefinedTrick` | Use the undefined trick when every field must be considered | D | | | Cannot tell where it is warranted |
| `HsPreferredLibraries` | sydtest, genvalidity, opt-env-conf, autodocodec, servant, yesod, persistent, monad-logger | B | layout | warn | Denylist the alternatives in build-depends |
| `HsStackYamlAtRoot` | `stack.yaml` at the repository root | B | layout | error | |
| `HsPackageDirMatchesName` | Each package in a directory matching its name, at the root | B | layout | error | |
| `HsSrcAndTestDirs` | Library code in `src/`, tests in `test/` | B | layout | error | |
| `HsAppOnlyMain` | `app/` contains only `main = specificMainHere` | B | syntax | error | Nothing in `app/` can be imported or tested |
| `HsTestSuiteNaming` | Test suite `<package>-test`, in `<package>-gen` | B | layout | error | Cabal metadata only |
| `HsSpecFilePerModule` | `src/Foo/Bar.hs` is tested in `test/Foo/BarSpec.hs` | C | layout | ratchet | Both directions. An untested module carries a file-scope annotation. High volume at adoption |
| `HsNoSectionHeadersInCode` | No `-- * section header` outside the export list | B | comment | error | |
| `HsOrmolu` | Use ormolu for formatting | A | | | |
| `HsRecordSyntaxForProducts` | Prefer record syntax when using a product type | B | project | warn | Positional construction of a constructor that has field names |

## Testing

| ID | Rule | Bucket | Tier | Class | Note |
|---|---|---|---|---|---|
| `TestRoundtripForSerialisation` | Roundtrip tests for any serialisation | C | project | error | Obligation, per instance-class-set table: `HasCodec`, `Binary`, `PersistField`, `ToHttpApiData` |
| `TestGoldenForExternalOutput` | Golden tests for any external output | C | project | warn | Cannot tell what leaves the system. Approximate by module location |
| `TestPropertyOverDummyValues` | Property testing instead of dummy values | D | | | Retired by the annotation model: a ratio has no site an annotation can attach to |
| `TestPreferPropertyTesting` | Prefer property testing in general | D | | | |
| `TestTestableTopLevel` | A helper with real logic becomes an exported top-level function | B | syntax | warn | Same check as `HsWhereHoldingLogic` |
| `TestParallelNoPollution` | All tests run in parallel, avoid test pollution | D | | | Not statically decidable |
| `TestJsonSpecPerJsonType` | `jsonSpec @T` for every `ToJSON` plus `FromJSON` type | C | project | error | Obligation |
| `TestTestLaws` | If code has laws, test them | C | project | error | Same engine as `HsInstancesLawAbiding` |
| `TestExactAssertions` | `shouldBe` over `shouldSatisfy` | B | syntax | warn | |
| `TestAssertWholeValues` | Assert whole values, not picked-out fields | B | syntax | warn | Two or more `shouldBe` on field selectors of the same expression in one do-block |
| `TestNoTestHelpers` | Avoid helper functions in test modules | B | syntax | error | Overlaps `HsTestOneSpecPerFile` |

## Nix

| ID | Rule | Bucket | Tier | Class | Note |
|---|---|---|---|---|---|
| `NixUseFlakes` | Use flakes | B | layout | error | `flake.nix` exists |
| `NixAgenixSecrets` | Use agenix for secrets | B | nix | warn | |
| `NixOverlayLocation` | Most packaging code in `nix/overlay.nix` | B | layout | warn | |
| `NixModuleNaming` | Modules in `nix/<service>-module.nix` | B | layout | warn | |
| `NixNoFlakeUtils` | Do not use flake-utils | B | nix | error | Flake inputs |
| `NixX86_64Only` | Stick to x86_64-linux unless required | B | nix | warn | |
| `NixNixpkgsFmt` | nixpkgs-fmt via pre-commit | A | config | error | Plus a meta-check that the hook is configured |
| `NixStatix` | statix via pre-commit | A | config | error | Plus hook meta-check |
| `NixDeadnix` | deadnix via pre-commit | A | config | error | Plus hook meta-check |

## Annotations, process and generated files

| ID | Rule | Bucket | Tier | Class | Note |
|---|---|---|---|---|---|
| `SecurityNoPlaintextSecrets` | Never commit secrets in plaintext | A | | | Delegate to a secret scanner rather than reimplement one |
| `MargRefHasTag` | Every `[check:ref]` has a matching `[check:tag]` | A | | | marginalia errors already |
| `MargWhenToAnnotate` | Annotate when types, tests and linters cannot enforce it | D | | | This tool shrinks the set of cases where that is true |
| `MargPreferCheckOverTagref` | Prefer `[check:tag]` when you want a review checklist | D | | | |
| `ProcSimplerSolutions` | Prefer simpler solutions | D | | | |
| `ProcNoMocking` | NO MOCKING | B | layout | warn | Mocking libraries in build-depends, plus `Mock` / `Stub` / `Fake` identifiers |
| `SyncCabalMatchesHpack` | `.cabal` in sync with `package.yaml` | A | | | hpack pre-commit hook |
| `SyncNixMatchesCabal` | `default.nix` in sync with `.cabal` | A | | | cabal2nix pre-commit hook |
| `LoopsDocumentedPerProject` | Each project has well-documented feedback loops | B | layout | warn | A documented loops section exists in the repo |
| `TagrefIntegrity` | Tags and refs pair up | A | | | tagref |
| `TagrefConnectCoupledCode` | Connect code that must change together | D | | | Knowing two things are coupled is the human part |

## Families

The rules above are not one piece of work each, because they group by *shape*
rather than by topic, and shape is what a combinator can factor out. Every rule
here belongs to exactly one family. Nothing asserts that the assignment stays
total and disjoint, so a rule added above without a family here is a drift a
reader has to catch.

**F1 CommentPattern** (7). A pattern list over comment text plus a message. Roughly
3 lines each.
`CommentChangelogNarrative`, `CommentUncheckedClaim`,
`CommentPositionalReference`, `CommentWorkaroundUpstreamReason`,
`ProseNoVerifyWord`, `ProseNoNotXButY`, `ProseNoEmdash`

**F2 CommentShape** (4). A predicate on one comment block's text or geometry.
`CommentWrap80`, `CommentOneSentencePerLine`, `CommentDecorativeBanner`,
`CommentCommentedOutCode`

**F3 CommentVersusCode** (8). Compares a comment block against the declaration it
is attached to. The refactor-safety family, and the one that most repays a good
combinator.
`CommentDanglingName`, `CommentHaddockListsCallers`,
`CommentRestatedLiteral`, `CommentArgumentPosition`,
`CommentDuplicatesSignature`, `CommentHaddockRestatesConstant`,
`CommentRestatesCode`, `CommentTestNarration`

**F4 CommentPlacement** (3). A predicate on attachment rather than on text.
`CommentSectionLabelInFunction`, `HsNoSectionHeadersInCode`,
`HsStrictFields`

**F5 DeclPredicate** (17). A predicate on one declaration. The largest family, so
its combinator matters most.
`HsNewtypeNotSynonym`, `HsMultilineRecord`,
`HsRecordFieldPrefix`, `HsLambdaCase`, `HsLetOverWhere`,
`HsOneLetPerBinding`, `HsLocalTypeSignatures`, `HsWhereHoldingLogic`,
`HsNoDomainBool`, `HsTextViaPack`, `HsNoSemigroupOnText`,
`HsAppOnlyMain`, `HsTestOneSpecPerFile`, `TestExactAssertions`,
`TestAssertWholeValues`, `TestNoTestHelpers`, `TestTestableTopLevel`

**F6 DeclOrder** (5). A relation over positions in the declaration list. One
combinator taking a comparator covers all of them.
`HsTypesAboveUses`, `HsInstanceAdjacentToType`,
`HsInstancePriorityOrder`, `HsTestSpecTopmost`,
`HsOrderEntrypointFirst`

**F7 Obligation** (6). A table row, and close to no code of its own beyond it.
`TestJsonSpecPerJsonType`,
`TestRoundtripForSerialisation`, `TestGoldenForExternalOutput`,
`TestTestLaws`, `HsInstancesLawAbiding`, `HsSpecFilePerModule`

**F8 Layout** (11). A predicate on the project model. No Haskell parsing.
`HsStackYamlAtRoot`, `HsPackageDirMatchesName`, `HsSrcAndTestDirs`,
`HsTestSuiteNaming`, `HsPreferredLibraries`,
`HsNoLenses`, `ProcNoMocking`, `LoopsDocumentedPerProject`,
`NixUseFlakes`, `NixOverlayLocation`, `NixModuleNaming`

**F9 ConfigContent** (6). A predicate on a config file's contents.
`HsHlintBansPartial`, `HsNoWeederExceptions`, `HsFewExtensions`,
`NixNoFlakeUtils`, `NixAgenixSecrets`, `NixX86_64Only`

**F10 ProjectIndex** (5). Needs cross-package tables. Bespoke, around 25 lines
each.
`CommentDanglingHaddockRef`, `CommentCopyPastedTwin`,
`HsTestDescribeOrder`, `HsClassesOnlyForPolymorphism`,
`HsRecordSyntaxForProducts`

**F11 Types** (3). Needs `.hie` enrichment.
`HsNoCatchAllOwnSum`, `HsMonomorphicOverConstraints`, `HsPreferText`

**F0 Parent** (1). No implementation; satisfied by its subchecks.
`CommentRefactorSafe`

