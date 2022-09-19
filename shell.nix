{ sources ? import ./nix/sources.nix
, nixpkgs ? sources.nixpkgs
, pkgs ? import ./nix/pkgs.nix { inherit nixpkgs sources; }
, pre-commit ? import ./nix/pre-commit.nix { inherit sources; }
}:
pkgs.haskell.lib.buildStackProject {
  name = "ical-shell";
  buildInputs = with pkgs; [
    (import sources.niv { }).niv
    vcal
    icalInterops.python-echo
    zlib
  ] ++ pre-commit.tools;
  shellHook = pre-commit.run.shellHook + ''
    export SPEC=${pkgs.rfc5545}
  '';
}
