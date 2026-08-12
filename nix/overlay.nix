final: prev:
with final.lib;
with final.haskell.lib;
let
  hopinionExe = justStaticExecutables final.haskellPackages.hopinion;

  # The builders, against this nixpkgs and this hopinion. Attached to the
  # executable below rather than exported from a flake, so that a consuming
  # repository needs the overlay and nothing else: `pkgs.hopinion` is both the
  # tool and everything that runs it.
  builders = import ./checks.nix {
    pkgs = final;
    hopinion = hopinionExe;
  };
in
{
  hopinion = hopinionExe.overrideAttrs (old: {
    passthru = (old.passthru or { }) // builders;
  });

  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (self: super:
      let
        # Its upper bound on text predates GHC 9.10's, which is the only thing
        # marking it broken. Nothing else about it minds.
        diagnose = doJailbreak (dontCheck (unmarkBroken super.diagnose));

        hopinionPkg = name:
          buildStrictly (overrideCabal
            (self.callPackage (../${name}) { })
            (_: {
              doHaddock = false;
              doCoverage = false;
              doHoogle = false;
            }));

        hopinionPackages = {
          hopinion = hopinionPkg "hopinion";
          hopinion-hie = hopinionPkg "hopinion-hie";
          hopinion-gen = hopinionPkg "hopinion-gen";
        };
      in
      {
        inherit hopinionPackages diagnose;
      } // hopinionPackages
    );
  });
}
