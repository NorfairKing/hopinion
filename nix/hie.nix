# Getting a Haskell package to leave its @.hie@ files behind, which is what lets
# a rule ask what a splice generated instead of abstaining for it.
#
# The shape is weeder-nix's, which solves the same problem for the same tool:
# three transforms that compose, so a repository already building with ide info
# can take only the piece it lacks. The difference is that hopinion needs the
# test code as well, always, because that is where an obligation is met.
{ pkgs }:
let
  inherit (pkgs) haskell;
in
rec {
  # Add a 'hie' output holding what the compiler wrote down about every module:
  # the .hie file for what a module names, and the .hi interface for what it
  # declares.
  #
  # One directory per component, because every component with a main-is declares
  # a module called Main and cabal writes each of them to Main.hie, so merging
  # the trees would leave one file standing for all of them. Eighteen packages
  # in the corpus have more than one such component.
  #
  # Each tree is rooted where a module's own path starts, so that A.B.C is
  # A/B/C.hie, because that is how a module is looked up. Cabal writes the .hie
  # files under a per-component extra-compilation-artifacts directory, so
  # keeping that prefix would leave every lookup missing; the interfaces are
  # already rooted that way in the component's own build directory.
  #
  # The .hie files are moved, since nothing reads them where they were. The
  # interfaces are copied, since the rest of the build is still using them.
  addHieOutput = pkg:
    (haskell.lib.overrideCabal pkg (old: {
      configureFlags = (old.configureFlags or [ ]) ++ [ "--ghc-options=-fwrite-ide-info" ];
      # Cabal will not be told to write them into $hie directly, since a
      # configure flag is not expanded against the output variables, so they are
      # collected afterwards from wherever it put them.
      postBuild = (old.postBuild or "") + ''
        mkdir -p $hie
        find . -type d -name hie -path '*extra-compilation-artifacts*' | sort | while read -r artifacts
        do
          component=$(dirname "$artifacts" \
            | sed -e 's|/extra-compilation-artifacts$||' -e 's|^\./dist/build||' -e 's|^/||' -e 's|/|-|g')
          target=$hie/''${component:-lib}
          mkdir -p "$target"
          ( cd "$artifacts"
            find . -name '*.hie' | while read -r found
            do
              mkdir -p "$target/$(dirname "$found")"
              mv "$found" "$target/$found"
            done
          )
          # The library's build directory holds every other component's
          # directory too, so its own interfaces are the ones not under a
          # component's -tmp.
          ( cd "$(dirname "$(dirname "$artifacts")")"
            find . -name '*.hi' -not -path '*-tmp/*' | while read -r found
            do
              mkdir -p "$target/$(dirname "$found")"
              cp "$found" "$target/$found"
            done
          )
        done
      '';
    })).overrideAttrs (old: {
      outputs = (old.outputs or [ ]) ++ [ "hie" ];
    });

  # Compile the test and benchmark code without running it.
  #
  # Cabal does not build test code unless tests are turned on, and a package set
  # that turns them off is the ordinary case. The test suite is where an
  # obligation is met, so without this a package's whole test tree is missing
  # from its artifacts. Not run, because nothing here wants the answer.
  buildTestsWithoutRunning = pkg:
    haskell.lib.overrideCabal pkg (_: {
      doCheck = true;
      doBenchmark = true;
      checkPhase = "";
    });

  # Nothing here wants the code, and the .hie files are the same either way.
  disableOptimisation = pkg:
    haskell.lib.appendConfigureFlags pkg [
      "--ghc-options=-O0"
      "--ghc-options=-fignore-interface-pragmas"
      "--ghc-options=-fomit-interface-pragmas"
    ];

  # The package set, with every package under check built to leave its artifacts
  # behind.
  #
  # The set is extended rather than each package overridden on its own, so that a
  # package depending on another under check builds against the same override
  # instead of against a second build of it.
  withArtifacts = { haskellPackages, packages }:
    haskellPackages.extend (_: super:
      pkgs.lib.listToAttrs (map
        (name: pkgs.lib.nameValuePair name
          (disableOptimisation (buildTestsWithoutRunning (addHieOutput super.${name}))))
        packages));
}
