# Rule proposal: a constructor names the primitives it holds

Written, shipped, and taken back out. What is here is what two repositories
taught, which is the part worth reading.

The code is not gone. `origin/scalar-keeps` is the last state it reached, and
`origin/hs-no-anonymous-scalar` is the commit it shipped as. Both carry the
`good.hs` and `bad.hs` resources, which were argued over line by line and are
the best record of what the rule meant.

## The rule

**A constructor of two or more fields holds no anonymous primitive**, where
anonymous means named by neither a record field nor a type of its own.

A primitive a constructor holds is named by a record field, by a type, or by
nothing, and the last leaves a reader counting arguments. A constructor of one
field needs nothing, because its name is that field's name. A constructor of
several names the whole of what it holds and no part of it.

Two fixes, both always available: a newtype, or record syntax, which names the
fields without asking for a type and does not change a single call site.

`Bool` was left to `HsNoDomainBool`. A `Maybe` of a primitive counted, a list
and a `NonEmpty` did not, and `[Char]` counted because it is `String` spelled
out and reading one spelling and not the other would be a way around the rule.
Declarations inside a Template Haskell bracket were not read.

## What it found

In hopinion: five sites. Three wanted types and got them, and those three are
worth copying out, because each removed a round trip through a bare primitive:
the project command's expected packages became `[PackageName]`, the module
command's extensions became `[ExtensionName]`, and an export list became
`[ExportEntry]`. Two were a position or a path beside a message GHC or Cabal
wrote, and got annotations.

In nix-ci, 2330 modules of production code: twenty-seven sites.

- **Eleven wanted a type**, and every one was a good finding. Two `Double`s that
  are quantiles. Four `Reject !ServerError !Text` across the Codeberg, GitHub,
  GitLab and Stripe webhook handlers, wanting one shared reason type. Three
  `Accept !ByteString !DeliveryGUID`, wanting one shared payload type.
  `PostErrorTooLong !Text !Int !Int`, whose three sibling constructors hold the
  same attribute name singly and go unflagged. `FundedRepoUrl !UrlRepoId !Text`,
  whose sibling constructors are already fully typed.
- **Three wanted record syntax**: single-constructor products whose own comments
  say "just a strict tuple".
- **Thirteen wanted a suppression.**

## Why it did not survive

Forty-eight percent of findings in the first repository that had not already
adopted the practice want a suppression. The proposal predicted about a
quarter, and predicted they would be numeric pairs like a start and an end.

The real residue is a different shape, and it is the commonest shape an error
type has: **a status or a name beside a message somebody else wrote.**

```
RequestTokenUnknownError !Text !Text        GitHub's error and error_description
CodebergDecodeError !String !String
StripeHttpError !Int !LB.ByteString         a status and a body
Refused !Int !String                        a status and what it said
NixCiEvalLineErrored !(NonEmpty Text) !Text an attribute path and nix's message
InvoiceProduceTypstError !Text !TypstCompileError
```

Wrapping relayed prose names the library it came from rather than anything the
project has a notion of, so the newtype the rule asks for is not an improvement
at those sites. There were two genuine pairs in the twenty-seven. The rest of
the residue is this.

## If it is tried again

One idea was measured only by hand and looks worth measuring properly:
**exempt the anonymous primitive when every other field of the constructor is
already a type.** That covers `Refused !Int !String`,
`InvoiceProduceTypstError !Text !TypstCompileError` and
`NixCiEvalLineErrored !(NonEmpty Text) !Text` in one stroke, and would have cut
the residue by roughly half.

It costs recall: `FundedRepoUrl !UrlRepoId !Text` is a real finding with exactly
that shape, and so is the webhook `Reject !ServerError !Text`, four times over.
So it trades one of the rule's best clusters for most of its worst. Whether that
is worth it is a measurement nobody has made.

The other lesson is about evidence. This rule shipped on five findings in a
repository that already had the practice it asks for, which told us nothing
about repositories that do not. Measure somewhere that has not adopted it, and
count what wants a suppression rather than what fires.
