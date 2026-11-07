let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  melby-client = pkgs.callPackage ../client/melby-client.nix {};
  melby-daemon = pkgs.callPackage ../daemon/melby-daemon.nix {};
  melby-nifs = pkgs.callPackage ../daemon/lib/melbyd/nifs/melby-nifs.nix {};
in
  {
    inherit melby-client melby-daemon melby-nifs;
  }
