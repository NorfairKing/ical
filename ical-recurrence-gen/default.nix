{ mkDerivation, base, containers, genvalidity
, genvalidity-containers, genvalidity-sydtest, genvalidity-time
, ical, ical-gen, ical-recurrence, lib, QuickCheck, sydtest
, sydtest-discover, time
}:
mkDerivation {
  pname = "ical-recurrence-gen";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base genvalidity genvalidity-containers ical ical-gen
    ical-recurrence
  ];
  testHaskellDepends = [
    base containers genvalidity-sydtest genvalidity-time ical ical-gen
    ical-recurrence QuickCheck sydtest time
  ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
