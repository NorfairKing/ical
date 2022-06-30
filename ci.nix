{ sources ? import ./nix/sources.nix
, nixpkgs ? sources.nixpkgs
, system ? builtins.currentSystem
, pkgs ? import ./nix/pkgs.nix { inherit sources nixpkgs system; }
}:
let
  pre-commit = import ./nix/pre-commit.nix { inherit sources; };

  versions = {
    "nixos-22_05" = sources.nixpkgs-22_05;
  };

  mkReleaseForVersion = version: nixpkgs:
    let
      p = import ./nix/pkgs.nix {
        inherit sources nixpkgs system;
      };

    in
    p.icalRelease.overrideAttrs (old: { name = "ical-release-${version}"; });
in
{
  release = (import ./nix/pkgs.nix { inherit sources; }).icalRelease;
  pre-commit-check = (import ./nix/pre-commit.nix { }).run;
  hoogle = pkgs.buildEnv {
    name = "ical-hoogle";
    paths = [ (pkgs.haskellPackages.ghcWithHoogle (ps: pkgs.lib.attrValues pkgs.icalPackages)) ];
  };
  shell = pkgs.symlinkJoin {
    name = "ical-shell";
    paths = (import ./shell.nix { inherit sources pkgs pre-commit; }).buildInputs;
  };
} // builtins.mapAttrs mkReleaseForVersion versions
