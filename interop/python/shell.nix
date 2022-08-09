{ sources ? import ../../nix/sources.nix
, nixpkgs ? sources.nixpkgs
, pkgs ? import ../../nix/pkgs.nix { inherit nixpkgs sources; }
, pre-commit ? import ../../nix/pre-commit.nix { inherit sources; }
}:
(pkgs.python3.withPackages (p: with p; [ icalendar ])).env
