{ mkDerivation, base, lib, validity }:
mkDerivation {
  pname = "thing";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base validity ];
  license = "unknown";
}
