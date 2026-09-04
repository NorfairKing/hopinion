# Plan

What is left to build, and what constrains how it gets built. rules.md is the
catalogue of rules not yet written; this is the order to write them in.

## What exists

Six rules, one at each of the three levels and then some: `CommentBareTodo`,
`HsNoCustomShowRead`, `HsNoFilePath` and `HsNoSemigroupOnText` at the module
level, `HsGenValidInGenPackage` at the package level,
`TestGenValidSpecPerGenValid` at the project level.

Under them: extension resolution and two parser passes, comment attachment,
declaration, instance and expression extraction, a fact store per package in
SQLite, a project layer that answers from those stores alone, suppression
parsing and matching, a report that draws findings against the source they point
at, and Nix builders that turn all of it into one derivation per package plus
one for the repository.

`.hie` and `.hi` artifacts are read, but only for what a splice generated. The
types tier is not built.

## What must stay true

Break one of these and the tool stops being worth running.

**A rule says there is something to fix, or there is not.** There is no third
answer. `CheckResult` carries findings and nothing else, and it must stay that
way: a checker that can say "I cannot tell" about code somebody has to decide
about is a checker whose silence means nothing.

**A suppression that answers for nothing is an error.** That is what makes
on-by-default with unlimited local escapes safe. Three ways one stops being
relevant, and each fails the run: it suppresses nothing, it suppresses more than
one finding at once, or it is written somewhere it cannot attach. Remove any of
those three and suppressions start accumulating silently, which is the failure
mode every ratchet has.

The first has two ways of being true and one complaint. A rule can run over a
file and report nothing the suppression could answer for, and a rule can be one
that is never run over that file at all: a file under a source directory no
component claims, or a preprocessor's input, whose Haskell only exists at build
time. The second is the quieter, since nothing there is parsed and so nothing
there is judged, but the verdict is the same one and the report does not
distinguish them. Which file it happened to be written in is the tool's problem;
what the reader is owed is which suppression suppresses nothing.

Reaching that verdict without a parser is a scan, and every step the scan can
share with the parser it shares: what announces a suppression, how the rule is
read out of it, and whether this run makes that rule. One the scan finds and
cannot name a rule for fails the way it would in a file that is read. A file
under no source directory at all is outside what a package run reads and is
therefore not covered.

**A suppression the report offers must be one the parser accepts.** The report
tells a reader what to write and where; if the parser then rejects it, the tool
has lied. `suppressionIsFileScoped` is the one place that decides, and both the
hint and the text it offers ask it.

**The artifacts are not optional, and coverage is asserted.** Given any
`--hie-directory`, every module read must have both a `.hie` and a `.hi` file,
or the run fails naming the module. An artifact tree missing one module is
indistinguishable at the point of use from no artifacts at all: both say
nothing.

**A module that does not parse fails the run.** It is not a module with no
declarations. The parse outcome is kept in the facts and the project layer
treats it as an error.

**Facts cross a process boundary; source does not.** A package is read once into
a store, and the project layer answers from those stores alone. That is what
makes the Nix side one derivation per package plus one for the repository.
`Hopinion.Report` is the half that crosses; `Hopinion.Report.Render` is the half
that needs source. Keep them apart.

**A rule owns its own table.** The migration, what it writes out of one module,
and the query it answers with. The envelope never learns what is in that table,
which is why adding a rule adds no case to anything central.

**The rule set is a value, and rule ids are open.** A repository runs these
rules, plus rules of its own, minus the ones it has decided against, so no
module can hold the list and no enum can hold the names. `ruleSet` refuses two
rules by one id and an id that is not one. A suppression naming a rule that has
been turned off is reported rather than left, which is the same rule as every
other kind of suppression that answers for nothing.

## The three levels

| Level | Sees | Nix |
|---|---|---|
| module | one module's source | inside the package derivation |
| package | every module of one package | one derivation per package |
| project | every module of every package | one derivation over the fact outputs |

## M1: Guards

The rules with near-zero volume in the repositories they were surveyed against.
Not cleanup, regression prevention. This milestone delivers real enforcement at
essentially zero adoption cost, which is why it goes first.

`ProcNoMocking`, `HsNewtypeNotSynonym`, `CommentDanglingHaddockRef`,
`HsPreferredLibraries`, `HsLambdaCase`, `CommentChangelogNarrative`,
`CommentUncheckedClaim`, `CommentDecorativeBanner`, `ProseNoVerifyWord`,
`ProseNoEmdash`.

`ProseNoEmdash` is the outlier: an em dash is common enough in prose that a
repository adopting this rule has hundreds. `[allow:ProseNoEmdash]` is more
absurd than fixing the character, so this milestone also carries **a rule's
right to refuse suppression**: a way for a rule to say so, a report that says
why the finding cannot be refused, and a suppression naming such a rule reported
as the mistake it is. Nothing else wants it, so it arrives with the rule that
does.

There are no severities to add that to. Every rule fails the run, and a
repository that does not want a rule's findings turns the rule off, so this is a
property of its own rather than a third value of something.

**Done when:** `nix flake check` passes and each rule has a finding count
recorded, with any divergence from the survey explained. A divergence is
expected and is information: the survey's regular expression undercounted, or
the parser sees something a regular expression could not.

## M2: The comment fixer

`CommentOneSentencePerLine` and `CommentWrap80` are the largest block of
adoption debt in the whole catalogue and are pure formatting. Repair them
instead of annotating them.

- `hopinion fix`, which reflows comment prose only. Never touches code, never
  rewrites words, never merges or splits a comment block.
- An abbreviation exclusion list, at minimum `e.g.`, `i.e.`, `vs.`, `etc.`, and
  module-qualified names in prose.
- `fix` is opt-in and never runs in CI.

**Done when:** running `fix` on a scratch copy of a repository leaves those two
rules at zero findings, `fix` is idempotent, and `git diff` on the result
contains no change outside comment lines. That last assertion is the one that
matters and it is mechanical: a diff touching a non-comment line is a bug.

## M3: Layout and config

No new machinery, so this is cheap, and it is where the standards are most
mechanically true.

`HsStackYamlAtRoot`, `HsPackageDirMatchesName`, `HsSrcAndTestDirs`,
`HsAppOnlyMain`, `HsTestSuiteNaming`, `HsSpecFilePerModule`,
`HsHlintBansPartial`, `HsNoWeederExceptions`, `HsFewExtensions`, `HsNoLenses`,
`LoopsDocumentedPerProject`, `NixUseFlakes`.

`HsSpecFilePerModule` ships as `ratchet`, and deciding what it means for a
package with no gen package is part of this milestone.

**Done when:** `nix flake check` passes and a run produces a per-package table
of layout findings that reads as a to-do list.

## M4: The comment tier

The bulk of the comment rules, once the fixer has removed the formatting noise
and M1 has proved the attachment pass on easy cases.

`CommentPositionalReference`, `HsNoSectionHeadersInCode`,
`CommentRestatedLiteral`, `CommentArgumentPosition`, `CommentDanglingName`,
`CommentDuplicatesSignature`, `CommentHaddockRestatesConstant`,
`CommentWorkaroundUpstreamReason`, `HsStrictFields`,
`CommentHaddockListsCallers`, `CommentSectionLabelInFunction`.

**Done when:** `nix flake check` passes, and every rule in this set has a
measured false positive rate on a hand-audited sample of 50 findings. A rule
above 10% goes back to `warn` and does not ship.

The hand audit is the only part of this plan that cannot be automated, and it is
what earns these rules the right to fail a build.

## M5: Declaration structure

Everything that needs the module-local declaration list and call graph.

`HsTypesAboveUses`, `HsInstanceAdjacentToType`, `HsInstancePriorityOrder`,
`HsOneLetPerBinding`, `HsMultilineRecord`, `HsLetOverWhere`,
`HsLocalTypeSignatures`, `HsRecordFieldPrefix`, `HsTestOneSpecPerFile`,
`HsTestSpecTopmost`, `TestNoTestHelpers`, `TestExactAssertions`,
`TestAssertWholeValues`, `HsWhereHoldingLogic`, `HsTextViaPack`,
`HsNoDomainBool`.

**Blocked on a measurement first.** Several of these were not surveyable by
regular expression, so their volume is unknown, and a rule with four thousand
findings changes this milestone's shape. The first task is to implement the
cheapest of them and count. `HsRecordFieldPrefix` and `HsStrictFields` are the
two most likely to be large.

`HsOrderEntrypointFirst` is deliberately **not** here. It needs a partial order
over the call graph, it is the one check tempted into auto-fix, and it should
wait until the rest of the structure tier is settled.

**Done when:** `nix flake check` passes and every rule in the set has a finding
count recorded.

## M6: The obligation engine

The highest-value milestone and the one with the most machinery, which is why it
is late rather than early despite the value.

- A project index built from per-package facts.
- Instance discovery across all five syntactic forms. Most instances in a
  codebase that uses generic deriving arrive through `deriving ... via`, so an
  engine that only understands `instance C T where` would miss most of the JSON
  instances it exists to check.
- `TestJsonSpecPerJsonType`, `TestRoundtripForSerialisation`,
  `TestGoldenForExternalOutput`, `HsInstancesLawAbiding`, `TestTestLaws`.
- Then the remaining project-tier rules: `HsTestDescribeOrder`,
  `CommentCopyPastedTwin`, `HsClassesOnlyForPolymorphism`,
  `HsRecordSyntaxForProducts`.

**Done when:** `nix flake check` passes, the property that a split run and a
whole-project run agree holds over a multi-package fixture, and the obligation
findings are reconciled against a hand count with every difference explained.

## M7: The types tier

`HsNoSemigroupOnText`, `HsNoCatchAllOwnSum`, `HsMonomorphicOverConstraints`,
`HsPreferText`, through the type information in `.hie` files.

What `HsNoSemigroupOnText` still wants from types is the concatenations with no
literal in them, which is where `T.pack x <> y` and `show x <> y` live, and
telling a `Builder` from a `Text` so it stops asking at the one place `<>` is
the right operator.

**Done when:** `nix flake check` passes with the types tier active, and the tool
run without artifacts states which rules it skipped.

## Not planned

Deliberately, with the reason:

- **`CommentRestatesCode` and `CommentTestNarration`.** The heuristic core. They
  need the tool to have earned trust, and they are the clearest case for a
  language model review pass rather than a linter.
- **The Nix rules.** A Haskell tool has no business parsing Nix. A separate
  small checker, later.
- **A query DSL for checks.** Only worth it if the rule count makes
  one-module-per-rule painful, which it has not yet.
- **Editor integration.** The `module` command is the seam it would use, so
  nothing needs designing now.

## Adding a rule

For a rule in a family that already has a combinator:

1. Write `Hopinion/Check/<Area>/<Name>.hs`, exporting only `rule`, with its id
   written as a literal in it.
2. Add one line to `builtinRules`.
3. Add `test_resources/Rule/<RuleId>/good.hs` and `bad.hs`. A `scenarioDir` runs
   one test per file, so a further case is a further file named `good*` or
   `bad*`, and still no test code.
4. Run the suite with `--golden-start`, which writes the golden of what the rule
   says about each of them: the finding's span, what it is about, and its
   message.
5. Run the tool over a corpus and record the finding count, which is what decides
   the shipping class.

Step 2 is the only edit outside the rule's own module and its resources. Adding
the module and not the line leaves a rule nothing runs, which the resource
listing test catches in both directions. Step 5 is the only step that needs
judgement.

A rule a repository writes for itself takes the same steps minus the second: it
goes in that repository's own executable, which calls `hopinionWith`.

### The budget

| | Target |
|---|---|
| Haskell for a rule in an existing family | 3 to 15 lines |
| Haskell for a rule needing bespoke logic | under 40 lines |
| Test code per rule | **zero** |
| Resource files per rule | 2, one clean and one dirty |
| Files touched outside the rule's own module | 1 |

If adding a rule needs more than that, either it belongs to a family with no
combinator yet, or it needs a fact that extraction does not produce. Both are
real costs, and both are paid once on behalf of every later rule of the same
shape.

The current ratio is against the budget: 6,856 lines of infrastructure carrying
689 lines of rules over six rules, where the design predicted roughly 1,300
carrying 55. The three cheapest rules are 29, 42 and 46 lines, which is the
budget holding once the facts a rule needs are already extracted; the average is
carried by the obligation rule at 375, which was deliberately the hardest thing
in the design. Whether the families are optimistic by an order is a question the
next few rules settle.

## Anti-goals

**No plugin system, no dynamic loading, no rule DSL.** The extension point is a
Haskell module and a recompile, because the person adding a rule is the person
who wrote the standard and works inside the repository.

**No type class for rules.** A class with one instance per rule buys nothing:
there is no generic code polymorphic in a rule type, so a record of functions is
the right encoding.

**No abstraction over the families themselves.** Combinators with different
signatures is correct. Unifying them behind one interface would add a layer that
every rule has to be understood through.

## Risks

**Comment attachment is the critical path.** M1, M2 and M4 all stand on it, and
it fails silently when wrong.

**M5 is unsized.** Several of its rules have no count. It could be twice the
size it looks.

**`HsInstancesLawAbiding` has no target yet.** The law-test combinators it would
look for are called nowhere, so the rule cannot ship until what a law test looks
like in this stack is chosen. Blocked, not merely unimplemented.
