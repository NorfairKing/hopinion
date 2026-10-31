{ mkDerivation, autodocodec-yaml, base, bytestring, containers
, filepath, genvalidity, genvalidity-aeson, genvalidity-containers
, genvalidity-path, genvalidity-sydtest, genvalidity-sydtest-aeson
, genvalidity-text, hopinion, hopinion-hie, lib, ormolu, path
, path-io, persistent, process, QuickCheck, safe-coloured-text
, sydtest, sydtest-aeson, sydtest-discover, text, yaml
}:
mkDerivation {
  pname = "hopinion-gen";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base genvalidity genvalidity-aeson genvalidity-containers
    genvalidity-path genvalidity-text hopinion QuickCheck text
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    autodocodec-yaml base bytestring containers filepath
    genvalidity-path genvalidity-sydtest genvalidity-sydtest-aeson
    genvalidity-text hopinion hopinion-hie path path-io persistent
    process QuickCheck safe-coloured-text sydtest sydtest-aeson text
    yaml
  ];
  testToolDepends = [ ormolu sydtest-discover ];
  license = "unknown";
  mainProgram = "hopinion-example";
}
