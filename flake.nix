{
  description = "ical";
  nixConfig = {
    extra-substituters = "https://ical.cachix.org";
    extra-trusted-public-keys = "ical.cachix.org-1:p7f+GXzQmwWs/h0Od3mQJNoB/8hJb86HjgHUtB4vF+c=";
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-24.05";
    nixpkgs-23_11.url = "github:NixOS/nixpkgs?ref=nixos-23.11";
    horizon-advance.url = "git+https://gitlab.horizon-haskell.net/package-sets/horizon-advance";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    validity.url = "github:NorfairKing/validity";
    validity.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    safe-coloured-text.flake = false;
    fast-myers-diff.url = "github:NorfairKing/fast-myers-diff";
    fast-myers-diff.flake = false;
    sydtest.url = "github:NorfairKing/sydtest";
    sydtest.flake = false;
    opt-env-conf.url = "github:NorfairKing/opt-env-conf";
    opt-env-conf.flake = false;
    conformance.url = "github:NorfairKing/conformance";
    conformance.flake = false;
    dekking.url = "github:NorfairKing/dekking";
    dekking.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , nixpkgs-23_11
    , horizon-advance
    , pre-commit-hooks
    , validity
    , safe-coloured-text
    , autodocodec
    , fast-myers-diff
    , sydtest
    , opt-env-conf
    , conformance
    , dekking
    }:
    let
      system = "x86_64-linux";
      nixpkgsFor = nixpkgs: import nixpkgs { inherit system; };
      pkgs = nixpkgsFor nixpkgs;
      allOverrides = pkgs.lib.composeManyExtensions [
        (pkgs.callPackage (validity + "/nix/overrides.nix") { })
        (pkgs.callPackage (autodocodec + "/nix/overrides.nix") { })
        (pkgs.callPackage (safe-coloured-text + "/nix/overrides.nix") { })
        (pkgs.callPackage (fast-myers-diff + "/nix/overrides.nix") { })
        (pkgs.callPackage (sydtest + "/nix/overrides.nix") { })
        (pkgs.callPackage (opt-env-conf + "/nix/overrides.nix") { })
        (pkgs.callPackage (conformance + "/nix/overrides.nix") { })
        (pkgs.callPackage (dekking + "/nix/overrides.nix") { })
        self.overrides.${system}
      ];
      horizonPkgs = horizon-advance.legacyPackages.${system}.extend allOverrides;
      haskellPackagesFor = nixpkgs: (nixpkgsFor nixpkgs).haskellPackages.extend allOverrides;
      haskellPackages = haskellPackagesFor nixpkgs;
    in
    {
      overrides.${system} = pkgs.callPackage ./nix/overrides.nix { };
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system}.default = haskellPackages.icalRelease;
      checks.${system} =
        let
          backwardCompatibilityCheckFor = nixpkgs: (haskellPackagesFor nixpkgs).icalRelease;
          allNixpkgs = {
            inherit
              nixpkgs-23_11;
          };
          backwardCompatibilityChecks = pkgs.lib.mapAttrs (_: nixpkgs: backwardCompatibilityCheckFor nixpkgs) allNixpkgs;
        in
        backwardCompatibilityChecks // {
          forwardCompatibility = horizonPkgs.icalRelease;
          coverage-report = haskellPackages.dekking.makeCoverageReport {
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
      devShells.${system}.default = haskellPackages.shellFor {
        name = "ical-shell";
        packages = p: builtins.attrValues p.icalPackages;
        withHoogle = true;
        doBenchmark = true;
        buildInputs = with pkgs; [
          cabal-install
          vcal
          zlib
        ] ++ self.checks.${system}.pre-commit.enabledPackages;
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
      nix-ci = {
        auto-update = {
          enable = true;
          base = "development";
        };
        cachix = {
          name = "ical";
          public-key = "ical.cachix.org-1:p7f+GXzQmwWs/h0Od3mQJNoB/8hJb86HjgHUtB4vF+c=";
        };
      };
    };
}
