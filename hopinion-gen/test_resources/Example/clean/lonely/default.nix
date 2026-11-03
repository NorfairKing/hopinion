{ mkDerivation, base, lib }:
mkDerivation {
  pname = "lonely";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  license = "unknown";
}
