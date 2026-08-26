let
  system = "x86_64-linux";
in
{
  auto-update = {
    enable = true;
    base = "development";
  };
  cachix = {
    name = "ical";
    public-key = "ical.cachix.org-1:p7f+GXzQmwWs/h0Od3mQJNoB/8hJb86HjgHUtB4vF+c=";
  };
  deploy = {
    release-to-hackage = {
      package = "packages.${system}.release-to-hackage";
      branches = [ "master" ];
      secrets = [ "HACKAGE_API_KEY" ];
    };
  };
}
