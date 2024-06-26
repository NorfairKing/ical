{ mkDerivation, base, base64, bytestring, case-insensitive
, conformance, containers, deepseq, dlist, lib, megaparsec
, network-uri, text, time, time-compat, validity
, validity-bytestring, validity-case-insensitive
, validity-containers, validity-network-uri, validity-text
, validity-time
}:
mkDerivation {
  pname = "ical";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base base64 bytestring case-insensitive conformance containers
    deepseq dlist megaparsec network-uri text time time-compat validity
    validity-bytestring validity-case-insensitive validity-containers
    validity-network-uri validity-text validity-time
  ];
  license = "unknown";
}
