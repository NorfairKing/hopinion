{
  description = "hopinion";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/80bdc1e5ce51f56b19791b52b2901187931f5353";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix/f799ae951fde0627157f40aec28dec27b22076d0";
    weeder-nix.url = "github:NorfairKing/weeder-nix/e0f0c0532babecb3e5726a1fbc1f89c1472c4cd4";
    weeder-nix.flake = false;
    validity.url = "github:NorfairKing/validity/456dfd9e4ef7906d33ce70f3f5fdf204a12cce8e";
    validity.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec/29b57dc2d425d732ce75143ecf3fc6411df0347b";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text/5d1187b7873a8a1e397245c7c0c6d9ab88765d67";
    safe-coloured-text.flake = false;
    fast-myers-diff.url = "github:NorfairKing/fast-myers-diff/78ac171911bac6a9aafc2c9a642012c5a0e731f0";
    fast-myers-diff.flake = false;
    sydtest.url = "github:NorfairKing/sydtest/27be1a6de567b76878511bd1d60392372c6accdf";
    sydtest.flake = false;
    opt-env-conf.url = "github:NorfairKing/opt-env-conf/de7ac850e960c28e5182810515a87fbe1d6daede";
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
            cabal2nix.excludes = testResources;
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
            packages = [ "hopinion" "hopinion-hie" "hopinion-gen" ];
          };
        in
        {
          hopinion = own;
        }
      )
      // import ./nix/e2e.nix {
        inherit pkgs;
        inherit (pkgs) hopinion;
        # Owned by Hopinion.ExampleSpec, which asserts in one process every
        # property the checks here rest on. One set of examples, two ways of
        # running it.
        examples = {
          clean = ./hopinion-gen/test_resources/Example/clean;
          dirty = ./hopinion-gen/test_resources/Example/dirty;
          decided = ./hopinion-gen/test_resources/Example/decided;
        };
        # A real package that generates code, for the one check that has to
        # watch the compiler's answers change the answer.
        generatingPackage = pkgs.haskellPackages.hopinion;
        # An executable that is not this one: it calls hopinionWith with a rule
        # of its own beside the shipped ones, which is what a repository adding
        # a rule writes. Owned by Hopinion.Rule.Gen, so the rule the binary
        # registers and the rule the checks look for are one value.
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
