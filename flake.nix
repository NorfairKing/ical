{
  description = "ical";
  nixConfig = {
    extra-substituters = "https://ical.cachix.org";
    extra-trusted-public-keys = "ical.cachix.org-1:p7f+GXzQmwWs/h0Od3mQJNoB/8hJb86HjgHUtB4vF+c=";
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-22.11";
    nixpkgs-22_05.url = "github:NixOS/nixpkgs?ref=nixos-22.05";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    validity.url = "github:NorfairKing/validity";
    validity.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    safe-coloured-text.flake = false;
    sydtest.url = "github:NorfairKing/sydtest";
    sydtest.flake = false;
    dekking.url = "github:NorfairKing/dekking";
    dekking.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-22_05
    , pre-commit-hooks
    , validity
    , safe-coloured-text
    , autodocodec
    , sydtest
    , dekking
    }:
    let
      system = "x86_64-linux";
      pkgsFor = nixpkgs: import nixpkgs {
        inherit system;
        overlays = [
          self.overlays.${system}
          (import (validity + "/nix/overlay.nix"))
          (import (autodocodec + "/nix/overlay.nix"))
          (import (safe-coloured-text + "/nix/overlay.nix"))
          (import (sydtest + "/nix/overlay.nix"))
          (import (dekking + "/nix/overlay.nix"))
        ];
      };
      pkgs = pkgsFor nixpkgs;
    in
    {
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system} = {
        default = pkgs.icalRelease;
        rfc5545 = pkgs.rfc5545;
      };
      checks.${system} =
        let
          backwardCompatibilityCheckFor = nixpkgs:
            let pkgs' = pkgsFor nixpkgs;
            in pkgs'.icalRelease;
          allNixpkgs = {
            inherit
              nixpkgs-22_05;
          };
          backwardCompatibilityChecks = pkgs.lib.mapAttrs (_: nixpkgs: backwardCompatibilityCheckFor nixpkgs) allNixpkgs;
        in
        backwardCompatibilityChecks // {
          coverage-report = pkgs.dekking.makeCoverageReport {
            name = "test-coverage-report";
            packages = [
              "ical"
              "ical-recurrence"
            ];
            coverage = [
              "ical-gen"
              "ical-recurrence-gen"
              "ical-interop-test"
            ];
          };
          pre-commit = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              hlint.enable = true;
              hpack.enable = true;
              ormolu.enable = true;
              nixpkgs-fmt.enable = true;
              nixpkgs-fmt.excludes = [ ".*/default.nix" ];
              cabal2nix.enable = true;
            };
          };
        };
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "ical-shell";
        packages = (p:
          (builtins.attrValues p.icalPackages)
        );
        withHoogle = true;
        doBenchmark = true;
        buildInputs = (with pkgs; [
          cabal-install
          niv
          vcal
          zlib
        ]) ++ builtins.attrValues pkgs.icalInterops ++ (with pre-commit-hooks.packages.${system};
          [
            hlint
            hpack
            nixpkgs-fmt
            ormolu
            cabal2nix
          ]);
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
    };
}
