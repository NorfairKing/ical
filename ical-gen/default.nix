{ mkDerivation, base, bytestring, case-insensitive, conformance
, conformance-gen, containers, criterion, deepseq, dlist
, genvalidity, genvalidity-bytestring, genvalidity-case-insensitive
, genvalidity-containers, genvalidity-criterion
, genvalidity-network-uri, genvalidity-sydtest, genvalidity-text
, genvalidity-time, genvalidity-vector, ical, lib, megaparsec
, network-uri, path, path-io, pretty-show, QuickCheck
, safe-coloured-text, safe-coloured-text-terminfo, sydtest
, sydtest-discover, text, time, validity-network-uri, vector
}:
mkDerivation {
  pname = "ical-gen";
  version = "0.0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring case-insensitive conformance conformance-gen
    containers dlist genvalidity genvalidity-bytestring
    genvalidity-case-insensitive genvalidity-containers
    genvalidity-network-uri genvalidity-sydtest genvalidity-text
    genvalidity-time ical QuickCheck sydtest text time
  ];
  executableHaskellDepends = [
    base containers path path-io safe-coloured-text
    safe-coloured-text-terminfo text vector
  ];
  testHaskellDepends = [
    base bytestring conformance conformance-gen containers dlist
    genvalidity genvalidity-sydtest genvalidity-text ical megaparsec
    network-uri path path-io pretty-show QuickCheck sydtest text time
    validity-network-uri
  ];
  testToolDepends = [ sydtest-discover ];
  benchmarkHaskellDepends = [
    base criterion deepseq genvalidity genvalidity-criterion
    genvalidity-vector ical QuickCheck vector
  ];
  license = "unknown";
  mainProgram = "ical-spec-coverage";
}
