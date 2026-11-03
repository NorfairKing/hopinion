{ mkDerivation, base, genvalidity, lib, validity }:
mkDerivation {
  pname = "lonely";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base genvalidity validity ];
  license = "unknown";
}
