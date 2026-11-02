# hopinion

Static analysis that mechanically enforces Haskell code review standards.
Read README.md before doing anything here.

## "Add a rule"

A rule in this repository is a `Rule` value, never a line of prose. Adding one
means a check under `hopinion/src/Hopinion/Check/`, with a `why.txt` and a
`fix.txt` beside its `Rule.hs`, an entry in `Hopinion.Rule.Registry`, and a good
and a bad resource under `hopinion-gen/test_resources/Rule/<RuleId>/`. A
request to add a rule is never a request to edit a style guide, this file, or
the user's global configuration.

`fix.txt` is what a finding shows: strict instructions, one sentence per line,
exact enough that an agent following them gets it right. Reasons and examples
of why go in `why.txt`, which only `explain` shows.

A rule that ships also has to hold here: `nix flake check` runs hopinion on
hopinion, so a new rule means fixing everything it finds in this repository.

## Feedback loops

1. `cabal build all`
2. `cabal test hopinion-gen --test-options='--ai-executor'`
3. `pre-commit run -a`
4. `nix flake check`

They run in the dev shell, so `nix develop --command ...` when direnv is not
loaded. New files have to be `git add`ed before `nix flake check`, which reads
the git tree rather than the working tree.
