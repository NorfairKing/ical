{ lib
, haskell
, symlinkJoin
, vcal
, callPackage
, ...
}:
with lib;
with haskell.lib;
self: super:
let
  python-echo = callPackage ../ical-interop-test/interop-packages/python { };
  conformancePackages =
    let
      conformancePkg = name:
        buildFromSdist (
          overrideCabal (self.callPackage (../${name}) { })
            (old: {
              doBenchmark = true;
              configureFlags = (old.configureFlags or [ ]) ++ [
                # Optimisations
                "--ghc-options=-O2"
                # Extra warnings
                "--ghc-options=-Wall"
                "--ghc-options=-Wincomplete-uni-patterns"
                "--ghc-options=-Wincomplete-record-updates"
                "--ghc-options=-Wpartial-fields"
                "--ghc-options=-Widentities"
                "--ghc-options=-Wredundant-constraints"
                "--ghc-options=-Wcpp-undef"
                "--ghc-options=-Werror"
                "--ghc-options=-Wno-deprecations"
              ];
              # Ugly hack because we can't just add flags to the 'test' invocation.
              # Show test output as we go, instead of all at once afterwards.
              testTarget = (old.testTarget or "") + " --show-details=direct";
            })
        );
    in
    {
      conformance = conformancePkg "conformance";
      conformance-gen = conformancePkg "conformance-gen";
    };
  icalPackages =
    let
      icalPkg = name:
        buildFromSdist (
          overrideCabal (self.callPackage (../${name}) { })
            (old: {
              doBenchmark = true;
              configureFlags = (old.configureFlags or [ ]) ++ [
                # Optimisations
                "--ghc-options=-O2"
                # Extra warnings
                "--ghc-options=-Wall"
                "--ghc-options=-Wincomplete-uni-patterns"
                "--ghc-options=-Wincomplete-record-updates"
                "--ghc-options=-Wpartial-fields"
                "--ghc-options=-Widentities"
                "--ghc-options=-Wredundant-constraints"
                "--ghc-options=-Wcpp-undef"
                "--ghc-options=-Werror"
                "--ghc-options=-Wno-deprecations"
              ];
              # Ugly hack because we can't just add flags to the 'test' invocation.
              # Show test output as we go, instead of all at once afterwards.
              testTarget = (old.testTarget or "") + " --show-details=direct";
            })
        );
    in
    {
      ical = icalPkg "ical";
      ical-gen = icalPkg "ical-gen";
      ical-recurrence = icalPkg "ical-recurrence";
      ical-recurrence-gen = icalPkg "ical-recurrence-gen";
      ical-interop-test =
        let
          interop-inputs = [
            vcal
            python-echo
          ];
        in
        overrideCabal (icalPkg "ical-interop-test") (old: {
          buildDepends = (old.buildDepends or [ ]) ++ interop-inputs;
          passthru.interop-inputs = interop-inputs;
        });
    };
in
{
  inherit icalPackages;
  inherit conformancePackages;
  # Tests fail
  timeout = unmarkBroken (dontCheck super.timeout);
  icalRelease = symlinkJoin {
    name = "ical-release";
    paths = attrValues self.icalPackages;
  };
  conformanceRelease = symlinkJoin {
    name = "conformance-release";
    paths = attrValues self.conformancePackages;
  };
} // icalPackages // conformancePackages
