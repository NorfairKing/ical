{ lib
, haskell
, symlinkJoin
, vcal
, icalInterops
, ...
}:
with lib;
with haskell.lib;
self: super:
let
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
      ical-interop-test = addBuildDepends (icalPkg "ical-interop-test") [
        vcal
        icalInterops.python-echo
      ];
      ical-recurrence = icalPkg "ical-recurrence";
      ical-recurrence-gen = icalPkg "ical-recurrence-gen";
    };
in
{
  inherit icalPackages;
  # Tests fail
  timeout = unmarkBroken (dontCheck super.timeout);
  icalRelease = symlinkJoin {
    name = "ical-release";
    paths = attrValues self.icalPackages;
  };
} // icalPackages

