{ mkDerivation, base, lib, mtl, safe-exceptions, text, transformers
}:
mkDerivation {
  pname = "haskell-readert";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base mtl safe-exceptions text transformers
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
