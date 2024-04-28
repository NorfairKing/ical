{ mkDerivation, base, bytestring, genvalidity-sydtest, ical
, ical-gen, lib, path, path-io, process, sydtest, sydtest-discover
, text, timeout
}:
mkDerivation {
  pname = "ical-interop-test";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring genvalidity-sydtest ical ical-gen path path-io
    process sydtest text timeout
  ];
  testToolDepends = [ sydtest-discover ];
  doHaddock = false;
  license = "unknown";
}
