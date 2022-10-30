{ mkDerivation, base, bytestring, case-insensitive, containers
, deepseq, dlist, lib, megaparsec, mtl, network-uri
, parser-combinators, text, time, time-compat, validity
, validity-bytestring, validity-case-insensitive
, validity-containers, validity-network-uri, validity-text
, validity-time
}:
mkDerivation {
  pname = "ical";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring case-insensitive containers deepseq dlist
    megaparsec mtl network-uri parser-combinators text time time-compat
    validity validity-bytestring validity-case-insensitive
    validity-containers validity-network-uri validity-text
    validity-time
  ];
  testHaskellDepends = [ base ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
