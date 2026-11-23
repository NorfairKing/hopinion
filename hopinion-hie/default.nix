{ mkDerivation, array, base, containers, ghc, lib, path, text }:
mkDerivation {
  pname = "hopinion-hie";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [ array base containers ghc path text ];
  license = "unknown";
}
