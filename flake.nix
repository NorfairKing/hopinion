{
  description = "hopinion";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-26.05";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    weeder-nix.url = "github:NorfairKing/weeder-nix";
    weeder-nix.flake = false;
    validity.url = "github:NorfairKing/validity";
    validity.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    safe-coloured-text.flake = false;
    fast-myers-diff.url = "github:NorfairKing/fast-myers-diff";
    fast-myers-diff.flake = false;
    sydtest.url = "github:NorfairKing/sydtest";
    sydtest.flake = false;
    opt-env-conf.url = "github:NorfairKing/opt-env-conf";
    opt-env-conf.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , pre-commit-hooks
    , weeder-nix
    , validity
    , autodocodec
    , safe-coloured-text
    , fast-myers-diff
    , sydtest
    , opt-env-conf
    }:
    let
      system = "x86_64-linux";
      # The test resources hold deliberately broken and deliberately unformatted
      # miniature repositories, so no hook that formats or generates may see
      # them.
      testResources = [ "^hopinion-gen/test_resources/" ];
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          (import ./nix/overlay.nix)
          (import (validity + "/nix/overlay.nix"))
          (import (autodocodec + "/nix/overlay.nix"))
          (import (safe-coloured-text + "/nix/overlay.nix"))
          (import (fast-myers-diff + "/nix/overlay.nix"))
          (import (sydtest + "/nix/overlay.nix"))
          (import (opt-env-conf + "/nix/overlay.nix"))
          (import (weeder-nix + "/nix/overlay.nix"))
        ];
      };
    in
    {
      packages.${system} = {
        default = pkgs.hopinion;
      };
      checks.${system} = {
        release = self.packages.${system}.default;
        shell = self.devShells.${system}.default;
        weeder-check = pkgs.weeder-nix.makeWeederCheck {
          weederToml = ./weeder.toml;
          packages = builtins.attrNames pkgs.haskellPackages.hopinionPackages;
        };
        hlint-check =
          let
            src = pkgs.lib.cleanSourceWith {
              src = ./.;
              filter = path: type:
                (type == "directory" || pkgs.lib.hasSuffix ".hs" (baseNameOf path))
                  && !(pkgs.lib.hasInfix "test_resources" (toString path));
            };
          in
          pkgs.runCommand "hlint-check" { nativeBuildInputs = [ pkgs.haskellPackages.hlint ]; } ''
            hlint --hint=${./.hlint.yaml} ${src}
            touch $out
          '';
        # The plan is a deliverable too, so its feedback loop lives in the same
        # command as everything else.
        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            hpack.enable = true;
            hpack.excludes = testResources;
            ormolu.enable = true;
            # The test resources encode exact layouts, which is the whole point
            # of them, so no formatter may touch them.
            ormolu.excludes = testResources;
            nixpkgs-fmt.enable = true;
            nixpkgs-fmt.excludes = [ ".*/default.nix" ];
            cabal2nix.enable = true;
            # The examples the end to end checks build are the exception, since
            # building them is how those checks get what a compiler wrote down.
            cabal2nix.excludes = [ "^hopinion-gen/test_resources/(?!Example/(clean|dirty)/)" ];
          };
        };
      }
      # hopinion on itself, through the same builders a consuming flake calls, so
      # that the path this repository runs is the path it ships.
      // (
        let
          own = pkgs.hopinion.makeHopinionCheck {
            src = ./.;
            # Named rather than discovered, because the example repositories under
            # hopinion-gen/test_resources are packages too and are deliberately
            # not this one's.
            packages = [ "hopinion" "hopinion-rules" "hopinion-hie" "hopinion-gen" ];
          };
        in
        {
          hopinion = own;
        }
      )
      // import ./nix/e2e.nix {
        inherit pkgs;
        inherit (pkgs) hopinion;
        # Owned by Hopinion.ExampleSpec, which asserts every property the checks
        # here rest on. One set of examples, checked by the Haskell suite and
        # by the derivations.
        examples = {
          clean = ./hopinion-gen/test_resources/Example/clean;
          dirty = ./hopinion-gen/test_resources/Example/dirty;
          decided = ./hopinion-gen/test_resources/Example/decided;
        };
        # A real package that generates code, for the one check that has to
        # watch the compiler's answers change the answer.
        generatingPackage = pkgs.haskellPackages.hopinion;
        # An executable that is not this one: its main names a library function
        # that calls hopinionWith with a rule of its own beside the shipped ones,
        # which is what a repository adding a rule writes. Owned by
        # Hopinion.Rule.Gen, so the rule the binary registers and the rule the
        # checks look for are one value.
        exampleTool = pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.hopinion-gen;
      };
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "hopinion-shell";
        packages = p: builtins.attrValues p.hopinionPackages;
        withHoogle = true;
        buildInputs = with pkgs; [
          cabal-install
          git
          haskellPackages.weeder
          ormolu
          zlib
        ] ++ self.checks.${system}.pre-commit.enabledPackages;
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
    };
}
