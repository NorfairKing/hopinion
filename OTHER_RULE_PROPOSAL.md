# Rule proposal: a scalar the project already named keeps that name

Written, shipped, and taken back out. What is here is what running it on a real
repository taught, which is the part worth reading.

The code is not gone: `origin/scalar-ambiguity` is the last state it reached,
and its four commits walk through how it got there. Nothing needs rebuilding
from scratch.

## The rule

**A record field whose name ends in the name of a scalar newtype the project
already defines, and whose type is that newtype's underlying primitive.**

The lexicon is the project's own newtypes rather than a list of words. That is
what made it look ready: the author has already decided this scalar deserves a
type and written the type down, so the finding is not an opinion about
domain-ness, only that one site went without what is already there.

Four filters stood between a candidate and a finding:

- The underlying primitive has to match, seen through `Maybe`, `[]` and
  `NonEmpty`, because the newtype goes inside the container.
- The newtype must not be the field's own owner, since `unHostname` is how
  `Hostname` is taken apart rather than a site that declined it.
- The site's component must reach the newtype's package, walked over the cabal
  files' `build-depends`. Without this it named a type in a package downstream
  of the site nine times in nineteen.
- The field's name must end in the type's name, word for word.

## What it cost to find out

It shipped on the strength of one finding in this repository, which it got
right. That was not evidence: hopinion already had the practice the rule asks
for, so the only site left was the one it was best at.

Pointed at nix-ci, 2330 modules of production code, it found eleven and **five
were right**. Three of the six wrong ones were bugs with mechanical fixes, all
three written and all three on `origin/scalar-ambiguity`:

- **A name the project declares twice names no type.** nix-ci has three
  `ClientSecret` newtypes, one per forge, because one forge's client secret is
  not another's. The rule said "use ClientSecret" and chose between them by the
  order their rows came back in.
- **A constructor a module keeps to itself cannot be the fix.** `Width` is
  exported without its constructor on purpose, so a panel can only have a width
  the grid has room for. Naming it asked for something no site could do.
- **Letters are not words.** Reading the word boundary off the field's letters
  alone had `gitUserName` holding a `Username`, because the letters are the
  same ones. Both names are split into words now, and this one is on `master`
  inside the commit that removed the rest.

## Why it was reverted

Three of the eight findings that survive those fixes are still wrong, and none
is wrong in a way a checker can see:

```
pushEventUserUsername :: Maybe Text          GitLab's username, not ours
mergeRequestEventUserUsername :: Maybe Text  GitLab's username, not ours
e2eOIDCSettingUsername :: Text               a login at a third-party issuer
cursorRowHeight :: Word                      grid units, not a panel's height
```

The shape is the same every time: **the field holds another system's data under
a name this project also uses.** `Username` there is a validated account name
with `parseUsername` beside it, so the rule's fix for a GitLab webhook payload
would have rejected payloads GitLab sends. `cursorRowHeight` is an accumulator
in the same `Word` units as `cursorX` and `cursorY`, neither of which is
flagged, and `Height` is a panel's declared height.

The premise was that a project's own newtypes are a lexicon. They are not: a
name does not identify a concept, and a project that integrates with three
forges and an identity provider will use one English word for four different
things. No filter here separates "our scalar" from "theirs", because the
difference is in what the data means rather than in how it is written.

## If it is tried again

- It is a `ratchet`, not an `error`. Three suppressions in eight findings is the
  honest cost, and that is what the class column is for.
- Same-package findings are the ones most likely to be about one concept, and
  they were 7 of 42 across six repositories. A rule restricted to them would be
  small and nearly always right, which may be the version worth having.
- Measure on a repository that has not already adopted the practice. That was
  the only measurement that predicted anything.

## What the dependency walk was worth

The rule brought a package dependency graph with it: `build-depends` read into
the project model, a `StoredDependency` table, and a reachability walk over
them. It went back out with the rule, because nothing else uses it yet and
weeder is right about that.

`HsPreferredLibraries`, `ProcNoMocking` and `HsNoLenses` all want it later, and
it is worth keeping in mind that it worked: it removed nine wrong findings out
of nineteen on its own.
