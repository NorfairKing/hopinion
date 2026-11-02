{ mkDerivation, autodocodec-yaml, base, containers, filepath
, genvalidity, genvalidity-aeson, genvalidity-containers
, genvalidity-path, genvalidity-sydtest, genvalidity-sydtest-aeson
, genvalidity-text, ghc-lib-parser, hopinion, hopinion-hie
, hopinion-rules, lib, ormolu, path, path-io, persistent, process
, QuickCheck, safe-coloured-text, syb, sydtest, sydtest-aeson
, sydtest-discover, text, yaml
}:
mkDerivation {
  pname = "hopinion-gen";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base genvalidity genvalidity-aeson genvalidity-containers
    genvalidity-path genvalidity-text ghc-lib-parser hopinion
    hopinion-rules QuickCheck text
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    autodocodec-yaml base containers filepath genvalidity-sydtest
    genvalidity-sydtest-aeson ghc-lib-parser hopinion hopinion-hie
    hopinion-rules path path-io persistent process safe-coloured-text
    syb sydtest sydtest-aeson text yaml
  ];
  testToolDepends = [ ormolu sydtest-discover ];
  license = "unknown";
  mainProgram = "hopinion-example";
}
