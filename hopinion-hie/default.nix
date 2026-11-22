{ mkDerivation, base, containers, ghc, lib, path, text }:
mkDerivation {
  pname = "hopinion-hie";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers ghc path text ];
  license = "unknown";
}
