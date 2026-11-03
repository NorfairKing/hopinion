{ mkDerivation, base, genvalidity, genvalidity-sydtest, lib
, sydtest, thing
}:
mkDerivation {
  pname = "thing-gen";
  version = "0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base genvalidity thing ];
  testHaskellDepends = [ base genvalidity-sydtest sydtest thing ];
  license = "unknown";
}
