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
