# The reusable check builders. Consumable by another flake, which is what makes
# adopting hopinion in a repository an additive change of about fifteen lines.
#
# Every derivation here either produces something or judges it, and only the
# judging one can fail. A derivation that fails takes its output with it, so a
# checker that failed by failing would be a checker whose report you cannot
# read; producing and judging are split so the report survives either way.
{ pkgs, hopinion }:
let
  inherit (pkgs) lib runCommand;
  hie = import ./hie.nix { inherit pkgs; };

  # Every extension a cabal file can name a module through.
  #
  # The preprocessor inputs are included because hopinion has an answer for
  # them: the module they declare exists and holds no Haskell. Filtering them
  # out instead produces a module the cabal file declares and nothing can find,
  # which the project layer reports as a hole in the facts.
  moduleExtensions = [ ".hs" ".lhs" ".x" ".y" ".hsc" ".chs" ];

  # Only this package's own sources, never the repository root, so that touching
  # an unrelated file invalidates nothing here.
  sourceOf = src: name: lib.cleanSourceWith {
    name = "${name}-source";
    src = src + "/${name}";
    filter = path: type:
      type == "directory"
      || lib.hasSuffix ".cabal" path
      || builtins.any (ext: lib.hasSuffix ext path) moduleExtensions;
  };

  # A package's sources as a directory. A package set holds them either as one
  # or as an sdist tarball, depending on how the set was built.
  sourceDirOf = name: src: runCommand "${name}-hopinion-sources" { } ''
    if [ -d ${src} ]
    then ln -s ${src} $out
    else
      mkdir -p $out
      tar -xzf ${src} -C $out --strip-components=1
    fi
  '';

  # One --hie-directory per component tree, expanded at build time because the
  # components a package has are in its cabal file rather than in anything Nix
  # read. A package's hie output holds one tree per component: see hie.nix for
  # why they are not merged.
  hieArguments = roots: ''
    hieArgs=""
    for root in ${lib.concatStringsSep " " roots}
    do
      for tree in "$root"/*
      do
        if [ -d "$tree" ]
        then hieArgs="$hieArgs --hie-directory $tree"
        fi
      done
    done
  '';

  # The executable these builders run.
  #
  # A package, so a repository with rules of its own passes an executable that
  # calls `hopinionWith`. A bare path works too. What matters is that all three
  # derivations of a check run the same one, since they are three processes over
  # one set of facts.
  exeOf = p: if builtins.isString p then p else lib.getExe p;

  # Where the repository says which rules it has decided against.
  #
  # Passed as a path rather than read here and turned into flags, so that one
  # parser reads it: this file exists so that the development loop and the
  # derivations cannot disagree about which rules run.
  #
  # Named rather than found, because each package derivation is handed one
  # package's subtree and a file at the repository root is not in it.
  choicesArgument = choices:
    if choices == null then "" else "--hopinion-file ${choices}";

  # The file beside a repository, or null when it has not written one.
  #
  # `hopinion check` finds this file for itself, being handed the repository;
  # the derivations are not, so this is that lookup for them. Named rather than
  # buried in `makeHopinionCheck`, since it is the line connecting what a
  # repository decided to what its check runs.
  choicesIn = src:
    let at = src + "/hopinion.yaml";
    in if builtins.pathExists at then at else null;

  toolCall = { exe, choices }:
    lib.concatStringsSep " " [ (exeOf exe) (choicesArgument choices) ];

  # One derivation per package, producing both halves at once: the findings its
  # own modules already yielded, and the facts the rules that can see further
  # asked to carry. There is no separate facts-only build, because this one
  # succeeds whatever it found.
  packageOutputFor = { name, src, hieRoots ? [ ], exe ? hopinion, choices ? null }:
    runCommand "${name}-hopinion" { } ''
      ${hieArguments hieRoots}
      ${toolCall { inherit exe choices; }} package \
        --root ${src} \
        --rel-prefix ${name} \
        $hieArgs \
        --out $out \
        ${src}
    '';

  packageOutput = src: name: packageOutputFor {
    inherit name;
    src = sourceOf src name;
  };

  # The sources are passed as well as the packages, so that a project finding is
  # shown against the code like every other finding. That costs nothing in
  # rebuilds: a package's output already changes whenever its sources do.
  #
  # The expected package list is passed explicitly, so a missing package is a
  # loud failure rather than a silently smaller check, and every package's
  # artifacts are too, or the project phase would answer a question differently
  # from how the package phase answered it.
  projectOutputFor = { subjects, exe ? hopinion, choices ? null }:
    runCommand "hopinion-project" { } ''
      ${hieArguments (lib.concatMap (s: s.hieRoots or [ ]) subjects)}
      ${toolCall { inherit exe choices; }} project \
        ${lib.concatMapStringsSep " " (s: "--package ${packageOutputFor (s // { inherit exe choices; })}") subjects} \
        ${lib.concatMapStringsSep " " (s: "--source ${s.name}=${s.src}") subjects} \
        $hieArgs \
        --expect-packages ${lib.concatStringsSep "," (map (s: s.name) subjects)} \
        --out $out
    '';

  projectOutput = src: packages:
    projectOutputFor { subjects = map (n: { name = n; src = sourceOf src n; }) packages; };

  # The only derivation that can fail, and it holds no work of its own: what it
  # reads is already built and already readable.
  #
  # The same executable that produced the reports. Judging needs no rules, since
  # it reads a report rather than the code, but a repository running an
  # executable of its own should not have its reports read back by a different
  # one.
  judge = { name, outputs, exe ? hopinion }: runCommand name { } ''
    ${exeOf exe} judge ${lib.concatStringsSep " " outputs}
    touch $out
  '';

  # One derivation over packages already reduced to a name, a source and
  # whatever artifacts there are.
  #
  # It produces the project report and depends on the judge that reads it, so
  # building it both fails on a finding and leaves the report where a person can
  # read it. The per-package outputs sit underneath, which is why changing a
  # single package rebuilds three derivations rather than all of them.
  checkOver = { name, subjects, exe ? hopinion, choices ? null }:
    let report = projectOutputFor { inherit subjects exe choices; };
    in
    runCommand name
      {
        buildInputs = [ (judge { name = "${name}-judge"; outputs = [ report ]; inherit exe; }) ];
      } ''
      ln -s ${report} $out
    '';
  # Everything a name in `packages` has to be, said before anything is built.
  #
  # A name is used as a directory under `src` and as an attribute of the package
  # set. Those are the two things a caller can get wrong, and bare Nix says only
  # `attribute 'X' missing` for either. At evaluation, so the answer arrives in
  # seconds rather than after a build.
  assertPackagesResolve = { src, packages, haskellPackages }:
    let
      cabalDirectoriesIn = dir:
        let entries = builtins.readDir dir;
        in builtins.filter
          (n:
            entries.${n} == "directory"
            && builtins.any (lib.hasSuffix ".cabal")
              (builtins.attrNames (builtins.readDir (dir + "/${n}"))))
          (builtins.attrNames entries);

      present = cabalDirectoriesIn src;
      missingDirectory = builtins.filter (n: !(builtins.pathExists (src + "/${n}"))) packages;
      missingAttribute = builtins.filter (n: !(haskellPackages ? ${n})) packages;

      # A name is much more often mistyped than invented, so the names that are
      # nearly it are worth more than the whole list of what exists.
      nearly = n: builtins.filter (p: lib.strings.levenshteinAtMost 3 n p) present;

      unlines = xs: lib.concatStringsSep "\n" xs;
    in
    if packages == [ ]
    then
      throw ''
        hopinion: makeHopinionCheck was given no packages.
        A check over nothing passes without having checked anything, so it is
        refused. Name the packages to check, as they are called both as
        directories under ${toString src} and in the package set.
        Directories there that hold a .cabal file: ${lib.concatStringsSep " " present}
      ''
    else if missingDirectory != [ ]
    then
      throw ''
        hopinion: makeHopinionCheck was given package names with no directory under ${toString src}.
        ${unlines (map (n: "  ${n}${lib.optionalString (nearly n != [ ]) ", did you mean: ${lib.concatStringsSep ", " (nearly n)}"}") missingDirectory)}
        Every directory there that holds a .cabal file: ${lib.concatStringsSep " " present}
      ''
    else if missingAttribute != [ ]
    then
      throw ''
        hopinion: makeHopinionCheck was given package names that are not in the package set.
        ${unlines (map (n: "  ${n}") missingAttribute)}
        Each name is looked up in `haskellPackages` as well as used as a
        directory, because the check needs what the compiler wrote down about
        that package and only the set that builds it has that. Pass the set that
        builds this repository, or leave the name out.
      ''
    else null;
in
{
  # makeHopinionCheck is the one to call, and checkOver is the way out when a
  # repository's layout is not one it can describe. Everything else is how those
  # two are built and how the end to end tests drive them into their failing
  # cases, which is not a promise to anybody.
  internals = {
    inherit sourceOf sourceDirOf packageOutput packageOutputFor projectOutput projectOutputFor judge hieArguments exeOf choicesIn;
    inherit (hie) addHieOutput;
  };

  # The way out. A repository whose packages are not one directory each under
  # one root describes its subjects itself and calls this; everything
  # makeHopinionCheck does is work out what to pass here.
  inherit checkOver;

  # What a consuming flake calls, and one derivation is what it gets. Building
  # it fails if the repository has anything to answer for, and succeeds with the
  # report at its output otherwise.
  #
  # The package set is not optional. What a rule can tell about a module that
  # generates code depends on having the compiler's answer about that module,
  # and a rule that abstains is a rule that ran and said nothing. The
  # repository's own tree is what hopinion reads, so that a finding's path is
  # the path its reader has, and the same package built with @-fwrite-ide-info@
  # is what says what a splice generated: taking that from the set that built
  # the code makes the reader and the compiler agree on a version by
  # construction. The cost is that a check waits for a build, including of the
  # test code, which is where an obligation is met.
  #
  # `packages` is named rather than discovered. A directory holding a .cabal
  # file is not the same thing as a package this repository wants checked: test
  # resources are little repositories of their own. Each name is looked up in
  # the package set anyway, so naming the list is naming it where it is already
  # needed, and a package left off is a decision somebody wrote down rather than
  # a directory a walk did not reach.
  #
  # Which rules run is not an argument here or anywhere: a repository writes
  # that in `hopinion.yaml` at its root, which `hopinion check` reads too, so
  # the cheapest feedback loop and the slowest cannot disagree. This finds the
  # file and hands each derivation its path; nothing here reads what it says.
  makeHopinionCheck =
    { name ? "hopinion"
    , src
    , packages
    , haskellPackages ? pkgs.haskellPackages
    , exe ? hopinion
    }:
    let
      resolves = assertPackagesResolve { inherit src packages haskellPackages; };
      built = hie.withArtifacts { inherit haskellPackages packages; };
    in
    lib.seq resolves (checkOver {
      inherit name exe;
      choices = choicesIn src;
      subjects = map
        (n: {
          name = n;
          src = sourceOf src n;
          hieRoots = [ built.${n}.hie ];
        })
        packages;
    });
}
