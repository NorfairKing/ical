{ mkDerivation, base, conformance, genvalidity-sydtest, lib
, sydtest, sydtest-discover
}:
mkDerivation {
  pname = "conformance-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base conformance sydtest ];
  testHaskellDepends = [
    base conformance genvalidity-sydtest sydtest
  ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
}
