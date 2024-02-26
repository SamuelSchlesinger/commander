{ mkDerivation, base, hspec, lib, mtl }:
mkDerivation {
  pname = "commandert";
  version = "0.1.2.0";
  src = ./.;
  libraryHaskellDepends = [ base mtl ];
  testHaskellDepends = [ base hspec mtl ];
  homepage = "https://github.com/SamuelSchlesinger/commander";
  description = "A monad for commanders";
  license = lib.licenses.mit;
}
