# hopinion

> This is not the law, just my hopinion.

Static analysis that mechanically enforces Haskell code review standards: the
comment, style and testing guides.

## Adding the check to a flake

Take the overlay, and one derivation checks the whole repository:

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-25.05";
    hopinion.url = "github:NorfairKing/hopinion";
    hopinion.flake = false;
  };

  outputs = { nixpkgs, hopinion, ... }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ (import (hopinion + "/nix/overlay.nix")) ];
      };
    in
    {
      checks.${system}.hopinion = pkgs.hopinion.makeHopinionCheck {
        src = ./.;
        packages = [ "foobar" "foobar-gen" ];
      };
    };
}
```

## Adding rules of your own

A rule is a `Rule` value from `hopinion`. Write it in a library of your
own that depends on `hopinion` and `hopinion-rules`, and give that library an
executable:

```haskell
main :: IO ()
main = hopinionWith (builtinRules ++ [myRule])
```

Pass that executable to `makeHopinionCheck` as `exe`.
The rules hopinion ships are written against the same API, so a rule never
needs a change to hopinion itself. A module rule is handed each module's parse
tree, and a package or project rule can ask for each module's `.hie` and `.hi`
contents. Each of those is read once, however many rules ask.
