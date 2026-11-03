{ mkDerivation, base, lib }:
mkDerivation {
  pname = "thing-gen";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  license = "unknown";
}
