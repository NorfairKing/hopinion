{ mkDerivation, base, containers, ghc, lib, path, text, validity
, validity-text
}:
mkDerivation {
  pname = "hopinion-hie";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base containers ghc path text validity validity-text
  ];
  license = "unknown";
}
