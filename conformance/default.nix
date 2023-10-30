{ mkDerivation, base, lib, mtl }:
mkDerivation {
  pname = "conformance";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base mtl ];
  license = "unknown";
}
