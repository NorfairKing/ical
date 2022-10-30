{ mkDerivation, base, bytestring, case-insensitive, containers
, dlist, genvalidity, genvalidity-bytestring
, genvalidity-case-insensitive, genvalidity-containers
, genvalidity-network-uri, genvalidity-sydtest, genvalidity-text
, genvalidity-time, ical, ical-gen, lib, megaparsec, network-uri
, path, path-io, process, QuickCheck, sydtest, sydtest-discover
, text, time, timeout
}:
mkDerivation {
  pname = "ical-interop-test";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring case-insensitive containers dlist genvalidity
    genvalidity-bytestring genvalidity-case-insensitive
    genvalidity-containers genvalidity-network-uri genvalidity-sydtest
    genvalidity-text genvalidity-time ical megaparsec network-uri
    QuickCheck sydtest text time
  ];
  testHaskellDepends = [
    base bytestring genvalidity-sydtest ical ical-gen path path-io
    process sydtest text timeout
  ];
  testToolDepends = [ sydtest-discover ];
  doHaddock = false;
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
