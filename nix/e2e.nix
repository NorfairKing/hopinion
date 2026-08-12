# End to end tests over the Nix machinery, on example projects.
#
# The Haskell test suite exercises the layers; this exercises the derivations
# that wire them together. Everything here is specific to running under Nix and
# invisible to `hopinion check`: the source filter, `--rel-prefix`,
# `--expect-packages`, and the two ways a repository chooses its own rules.
#
# The shape under test is that producing a report always succeeds and only
# judging one fails, so the assertions come in pairs: the report of a dirty
# project is built and read, and the judgement over it is watched failing.
{ pkgs, hopinion, examples, generatingPackage, exampleTool }:
let
  inherit (pkgs) lib runCommand;

  # A derivation that succeeds exactly when the script inside it fails, and
  # whose output holds what the script said so the next test can read it.
  #
  # The script goes to a file first: redirecting an interpolated script directly
  # puts the redirect on its own line whenever that script spans more than one,
  # and the status read back is then the redirect's rather than the tool's.
  expectFailure = name: script: runCommand name { } ''
    cat > script.sh <<'HOPINION_E2E_SCRIPT'
    ${script}
    HOPINION_E2E_SCRIPT
    set +e
    bash script.sh > output 2>&1
    status=$?
    set -e
    if [ "$status" = 0 ]; then
      echo "Expected a non-zero exit, got 0. Output was:"
      cat output
      exit 1
    fi
    cp output $out
  '';

  expectOutputContains = name: needle: produced: runCommand name { } ''
    if ! grep -qF ${lib.escapeShellArg needle} ${produced}; then
      echo "Expected the output to contain: ${needle}"
      echo "It was:"
      cat ${produced}
      exit 1
    fi
    touch $out
  '';

  # Named the way a consuming flake names its own, and held to being complete
  # against the fixtures by the check below, so a package added to an example
  # and left off here fails rather than going unchecked.
  cleanPackages = [ "lonely" "thing" "thing-gen" ];
  dirtyPackages = [ "lonely" "thing" "thing-gen" ];

  # Every check of a project that is clean must pass, over the builder every
  # entry point goes through.
  #
  # Not `makeHopinionCheck`, because these example packages are in no package set
  # and are never built. So this is also the one place the builders run with no
  # compiler answers at all, which is what a repository with an unbuildable
  # package would hit.
  cleanCheck = hopinion.checkOver {
    name = "e2e-clean";
    subjects = map (n: { name = n; src = hopinion.internals.sourceOf examples.clean n; }) cleanPackages;
  };

  dirtySubjects = map (n: { name = n; src = hopinion.internals.sourceOf examples.dirty n; }) dirtyPackages;

  # Built, not judged, so it exists to be read even though the project it
  # describes is dirty. This is the property the split is for.
  dirtyPackageReport = hopinion.internals.packageOutput examples.dirty "thing";
  dirtyProjectReport = hopinion.internals.projectOutput examples.dirty dirtyPackages;

  # What a repository writes to decide against a rule.
  #
  # Written here rather than kept beside the example, because the example is
  # also what the Haskell suite runs over in one process and it must stay a
  # project with every rule against it.
  choicesTurningOffBareTodo = pkgs.writeText "hopinion.yaml" ''
    disabled-rules:
      - CommentBareTodo
  '';

  # The same dirty project with the module rule turned off. Built rather than
  # judged, like the report above, so the assertion can read what it says.
  withoutBareTodo = hopinion.internals.projectOutputFor {
    subjects = dirtySubjects;
    choices = choicesTurningOffBareTodo;
  };

  # And the same project checked by an executable that is not this one: the
  # example binary, which calls hopinionWith with a rule of its own beside the
  # shipped ones, which is the whole of what "a repository adds a rule" is.
  withExtraRule = hopinion.internals.projectOutputFor {
    subjects = dirtySubjects;
    exe = exampleTool;
  };

  # The one line that connects what a repository decided to what its check runs,
  # asked in both directions. It is the only part of the file's path into a
  # build that `hopinion check` does not walk itself, so nothing else covers it.
  choicesFound = hopinion.internals.choicesIn examples.decided;
  choicesAbsent = hopinion.internals.choicesIn examples.clean;

  judgeOf = reports:
    "${hopinion}/bin/hopinion judge ${lib.concatStringsSep " " reports}";
in
{
  e2e-clean = cleanCheck;

  # The package list is written down rather than discovered, so what can go
  # wrong is that it stops matching the repository it names: a package added to
  # a fixture and left off the list is a package silently not checked.
  e2e-named-packages-are-every-package = runCommand "e2e-named-packages-are-every-package" { } ''
    for pair in "${examples.clean}:${lib.concatStringsSep " " cleanPackages}" \
                "${examples.dirty}:${lib.concatStringsSep " " dirtyPackages}"
    do
      root="''${pair%%:*}"
      named="''${pair#*:}"
      present=$(cd "$root" && find . -name '*.cabal' -printf '%h\n' | sed 's|^\./||' | sort | tr '\n' ' ')
      named=$(printf '%s\n' $named | sort | tr '\n' ' ')
      if [ "$present" != "$named" ]; then
        echo "In $root the packages present are [$present] and the ones named are [$named]"
        exit 1
      fi
    done
    touch $out
  '';

  # A report of a dirty project is a build that succeeds. Nothing else in this
  # file means anything if that is not true, because every assertion below reads
  # a report that this derivation had to produce.
  e2e-dirty-report-is-readable = runCommand "e2e-dirty-report-is-readable" { } ''
    if [ ! -s ${dirtyPackageReport}/report.txt ]; then
      echo "The report of a dirty package is empty, or not there at all."
      exit 1
    fi
    if [ ! -s ${dirtyProjectReport}/report.json ]; then
      echo "The project report of a dirty project is empty, or not there at all."
      exit 1
    fi
    touch $out
  '';

  # A repository that wrote the file has it found, and one that did not has
  # nothing found for it. Both, because a lookup that always answered would hand
  # every check a path to a file that is not there, and one that never answered
  # would run every rule over a repository that had decided against one.
  e2e-choices-are-found-beside-the-repository =
    runCommand "e2e-choices-are-found-beside-the-repository" { } ''
      # Interpolated rather than `toString`, which yields the path without
      # depending on it, so the file would not be in the sandbox to read.
      if ! grep -q 'disabled-rules' ${choicesFound}; then
        echo "What was found beside a repository is not the file it wrote."
        exit 1
      fi
      if [ ${if choicesAbsent == null then "none" else "something"} != none ]; then
        echo "A repository holding no hopinion.yaml had something found beside it."
        exit 1
      fi
      touch $out
    '';

  # A rule the repository has decided against must actually not run, over the
  # derivations rather than in one process. Both directions, because a check
  # that reported nothing for some other reason would satisfy the first half on
  # its own.
  e2e-disabled-rule-is-not-run = runCommand "e2e-disabled-rule-is-not-run" { } ''
    if ! grep -q 'CommentBareTodo' ${dirtyProjectReport}/report.txt; then
      echo "The dirty project no longer reports CommentBareTodo, so this proves nothing."
      cat ${dirtyProjectReport}/report.txt
      exit 1
    fi
    if grep -q 'CommentBareTodo' ${withoutBareTodo}/report.txt; then
      echo "CommentBareTodo is turned off in hopinion.yaml and was reported anyway:"
      cat ${withoutBareTodo}/report.txt
      exit 1
    fi
    touch $out
  '';

  # A rule this tool does not ship, found by an executable that is not this one.
  # The example binary is what a repository adding a rule writes, so this is the
  # path a repository would take, walked.
  e2e-added-rule-is-run = runCommand "e2e-added-rule-is-run" { } ''
    if grep -q 'ExampleNoShouting' ${dirtyProjectReport}/report.txt; then
      echo "hopinion itself reports a rule it does not ship, so this proves nothing."
      exit 1
    fi
    if ! grep -q 'ExampleNoShouting' ${withExtraRule}/report.txt; then
      echo "An executable registering a rule of its own did not report it:"
      cat ${withExtraRule}/report.txt
      exit 1
    fi
    touch $out
  '';

  e2e-dirty-package-is-judged-failing =
    expectFailure "e2e-dirty-package-is-judged-failing" (judgeOf [ dirtyPackageReport ]);

  e2e-dirty-project-is-judged-failing =
    expectFailure "e2e-dirty-project-is-judged-failing" (judgeOf [ dirtyProjectReport ]);

  # Each package derivation sees a store path rather than the repository, so
  # without --rel-prefix every reported path would name a store path nobody can
  # navigate to.
  e2e-paths-are-repository-relative =
    expectOutputContains "e2e-paths-are-repository-relative"
      "thing/src/Thing.hs"
      "${dirtyPackageReport}/report.txt";

  # Anywhere in the line, not only at the start of one: a finding's path is
  # indented inside its report, so anchoring would make this pass on any output
  # at all.
  e2e-no-store-paths-in-findings = runCommand "e2e-no-store-paths-in-findings" { } ''
    if grep -q '/nix/store' ${dirtyPackageReport}/report.txt; then
      echo "A finding named a store path:"
      cat ${dirtyPackageReport}/report.txt
      exit 1
    fi
    touch $out
  '';

  # Absent input is an error, never an empty set, or the project layer would
  # silently conclude an obligation is met by code it never read. It fails by
  # being judged rather than by failing to run, so the message is read out of
  # the report the run produced.
  e2e-withheld-facts-fail =
    let
      # Exactly one package's facts withheld, so the message has to name that
      # one rather than whichever happens to be checked first.
      supplied = lib.filter (n: n != "lonely") dirtyPackages;
      withheld = runCommand "e2e-withheld-facts-report" { } ''
        ${hopinion}/bin/hopinion project \
          ${lib.concatMapStringsSep " " (n: "--package ${hopinion.internals.packageOutput examples.dirty n}") supplied} \
          --expect-packages ${lib.concatStringsSep "," dirtyPackages} \
          --out $out
      '';
    in
    expectOutputContains "e2e-withheld-facts-fail"
      "No facts for expected package lonely"
      "${withheld}/report.txt";

  # And the project layer refuses to conclude anything from nothing at all.
  e2e-no-packages-is-judged-failing =
    let
      empty = runCommand "e2e-no-packages-report" { } ''
        ${hopinion}/bin/hopinion project --expect-packages "" --out $out
      '';
    in
    expectFailure "e2e-no-packages-is-judged-failing" (judgeOf [ empty ]);

  # And judging nothing at all is a failure, for the reason judging a report of
  # nothing is. A caller that passes no reports has wired something up wrong,
  # and answering "clean" is the one answer that hides it: the reports it meant
  # to hand over are never read and the check goes green having read nothing.
  e2e-judging-nothing-fails =
    expectFailure "e2e-judging-nothing-fails" (judgeOf [ ]);

  # The artifact path, watched being consulted per module.
  #
  # A complete tree is accepted and a tree one module short is refused, which is
  # the pair: the second half alone would pass for a run that ignored the tree
  # and failed for some other reason, and the first half alone would pass for a
  # run that never looked. That the files are also parsed rather than only
  # counted is 'Hopinion.HieSpec', which reads what a compiler really wrote.
  e2e-artifacts-are-read =
    let
      name = generatingPackage.pname;
      src = hopinion.internals.sourceDirOf name generatingPackage.src;
      whole = (hopinion.internals.addHieOutput generatingPackage).hie;
      report = runCommand "e2e-artifacts-are-read-report" { } ''
        ${hopinion.internals.hieArguments [ whole ]}
        ${hopinion}/bin/hopinion project \
          --package ${hopinion.internals.packageOutputFor { inherit name src; hieRoots = [ whole ]; }} \
          --source ${name}=${src} $hieArgs \
          --expect-packages ${name} \
          --out $out
      '';
    in
    runCommand "e2e-artifacts-are-read" { } ''
      if grep -q 'No .hie file for' ${report}/report.txt; then
        echo "A run given a complete artifact tree says a module is missing from it."
        cat ${report}/report.txt
        exit 1
      fi
      touch $out
    '';

  # A build that covers less than was read from it is a failure, not a quieter
  # check. The two ways of learning nothing about a module look identical in
  # every report: the build never compiled it, which is a hole, or there was no
  # build at all, which is a mode.
  #
  # The hole is made rather than waited for: one module's answer is deleted from
  # a tree that was complete, which is what a package set that does not compile
  # test code leaves behind.
  e2e-incomplete-artifacts-are-a-failure =
    let
      name = generatingPackage.pname;
      src = hopinion.internals.sourceDirOf name generatingPackage.src;
      short = runCommand "hopinion-hie-missing-one" { } ''
        cp -r --no-preserve=mode,ownership ${(hopinion.internals.addHieOutput generatingPackage).hie} $out
        find $out -name 'Store.hie' -delete
      '';
      report = runCommand "e2e-incomplete-artifacts-report" { } ''
        ${hopinion.internals.hieArguments [ short ]}
        ${hopinion}/bin/hopinion project \
          --package ${hopinion.internals.packageOutputFor { inherit name src; hieRoots = [ short ]; }} \
          --source ${name}=${src} $hieArgs \
          --expect-packages ${name} \
          --out $out
      '';
    in
    expectOutputContains "e2e-incomplete-artifacts-are-a-failure"
      "No .hie file for hopinion/src/Hopinion/Store.hs"
      "${report}/report.txt";

  # A source mapping that names nothing is the same silent degradation as a
  # missing fact file, one flag over, so it is a complaint rather than a quiet
  # report with nothing under it.
  e2e-wrong-source-mapping-is-a-complaint =
    let
      misdirected = runCommand "e2e-wrong-source-report" { } ''
        ${hopinion}/bin/hopinion project \
          ${lib.concatMapStringsSep " " (n: "--package ${hopinion.internals.packageOutput examples.dirty n}") dirtyPackages} \
          --expect-packages ${lib.concatStringsSep "," dirtyPackages} \
          --out $out
      '';
    in
    expectOutputContains "e2e-wrong-source-mapping-is-a-complaint"
      "so it cannot be shown against its code"
      "${misdirected}/report.txt";
}
