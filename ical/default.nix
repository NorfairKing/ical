{ mkDerivation, base, bytestring, case-insensitive, containers
, deepseq, dlist, lib, megaparsec, mtl, network-uri, text, time
, time-compat, validity, validity-case-insensitive
, validity-containers, validity-network-uri, validity-text
, validity-time
}:
mkDerivation {
  pname = "ical";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base bytestring case-insensitive containers deepseq dlist
    megaparsec mtl network-uri text time time-compat validity
    validity-case-insensitive validity-containers validity-network-uri
    validity-text validity-time
  ];
  license = "unknown";
}
