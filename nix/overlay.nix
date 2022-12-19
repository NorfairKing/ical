final:
prev:
with final.haskell.lib;
{

  icalRelease =
    final.symlinkJoin {
      name = "ical-release";
      paths = final.lib.attrValues final.haskellPackages.icalPackages;
    };

  icalInterops = {
    python-echo = final.callPackage ../ical-interop-test/interop-packages/python { };
  };

  rfc5545 = final.linkFarmFromDrvs "rfc5545" [
    (final.fetchurl {
      url = "https://www.rfc-editor.org/rfc/rfc5545.txt";
      hash = "sha256-wlb4CUedmKoj1xu9FlizgA6p8T9BylblnI0t4bMcv8s=";
    })
    (final.fetchurl {
      url = "https://www.rfc-editor.org/rfc/pdfrfc/rfc5545.txt.pdf";
      hash = "sha256-CKaGyuqXUwmCKqqWS/cIFJ5u5Pu10e/coh7KMqkmDQU=";
    })
    (final.fetchurl {
      url = "https://www.rfc-editor.org/rfc/rfc5545.html";
      hash = "sha256-6bhmOfGmDONCkWyapkcaQVCbiFCxWQNC/jIXe0InkwU=";
    })
    # TODO get individual erata in case more are found.
    # TODO get the other relevant rfcs
    # (final.fetchurl {
    #   url = "https://www.rfc-editor.org/errata/rfc5545";
    #   hash = "sha256-hSHdXXCd1DmU3v8wK9DkGuP1DTQffLPhJVguod5lMEc=";
    # })
  ];

  haskellPackages = prev.haskellPackages.override (
    old: {
      overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
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
                  final.vcal
                  final.icalInterops.python-echo
                ];
                ical-recurrence = icalPkg "ical-recurrence";
                ical-recurrence-gen = icalPkg "ical-recurrence-gen";
              };
          in
          {
            inherit icalPackages;
            # Tests fail
            timeout = unmarkBroken (dontCheck super.timeout);
          } // icalPackages
      );
    }
  );
}
