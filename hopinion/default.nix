{ mkDerivation, aeson, autodocodec, autodocodec-schema, base
, bytestring, Cabal-syntax, conduit, containers, diagnose
, esqueleto, filepath, ghc-lib-parser, ghc-lib-parser-ex
, hopinion-hie, http-api-data, lib, monad-logger, mtl, opt-env-conf
, path, path-io, path-pieces, persistent, persistent-sqlite
, prettyprinter, prettyprinter-ansi-terminal, resourcet
, safe-coloured-text, safe-coloured-text-terminfo, text, unliftio
, unordered-containers, validity, validity-aeson
, validity-containers, validity-path, validity-text, yaml
}:
mkDerivation {
  pname = "hopinion";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson autodocodec autodocodec-schema base bytestring Cabal-syntax
    conduit containers diagnose esqueleto filepath ghc-lib-parser
    ghc-lib-parser-ex hopinion-hie http-api-data monad-logger mtl
    opt-env-conf path path-io path-pieces persistent persistent-sqlite
    prettyprinter prettyprinter-ansi-terminal resourcet
    safe-coloured-text safe-coloured-text-terminfo text unliftio
    unordered-containers validity validity-aeson validity-containers
    validity-path validity-text yaml
  ];
  executableHaskellDepends = [ base ];
  license = "unknown";
  mainProgram = "hopinion";
}
