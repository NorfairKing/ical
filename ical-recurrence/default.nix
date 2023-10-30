{ mkDerivation, base, conformance, containers, ical, lib, mtl, time
, validity
}:
mkDerivation {
  pname = "ical-recurrence";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base conformance containers ical mtl time validity
  ];
  license = "unknown";
}
