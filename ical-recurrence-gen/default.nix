{ mkDerivation, base, bytestring, conformance, conformance-gen
, containers, genvalidity, genvalidity-containers
, genvalidity-sydtest, genvalidity-time, ical, ical-gen
, ical-recurrence, lib, path, path-io, QuickCheck, sydtest
, sydtest-discover, text, time
}:
mkDerivation {
  pname = "ical-recurrence-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base conformance-gen genvalidity genvalidity-containers ical-gen
    ical-recurrence
  ];
  testHaskellDepends = [
    base bytestring conformance conformance-gen containers
    genvalidity-sydtest genvalidity-time ical ical-gen ical-recurrence
    path path-io QuickCheck sydtest text time
  ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
}
