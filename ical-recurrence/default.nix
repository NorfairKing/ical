{ mkDerivation, base, containers, ical, lib, mtl, time, validity }:
mkDerivation {
  pname = "ical-recurrence";
  version = "0.0.0.0";
  src = ./.;
  libraryHaskellDepends = [ base containers ical mtl time validity ];
  license = "unknown";
}
