{ mkDerivation, aeson, base, katip, lib, mtl, safe-exceptions, stm
, tasty, tasty-hunit, text, transformers
}:
mkDerivation {
  pname = "haskell-readert";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base katip mtl safe-exceptions text transformers
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    aeson base katip mtl safe-exceptions stm tasty tasty-hunit text
    transformers
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
