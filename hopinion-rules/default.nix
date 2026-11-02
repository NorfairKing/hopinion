{ mkDerivation, base, containers, esqueleto, ghc-lib-parser
, hopinion, lib, path, persistent, syb, text, validity
, validity-text
}:
mkDerivation {
  pname = "hopinion-rules";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers esqueleto ghc-lib-parser hopinion path persistent
    syb text validity validity-text
  ];
  executableHaskellDepends = [ base ];
  license = "unknown";
  mainProgram = "hopinion";
}
