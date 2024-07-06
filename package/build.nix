let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  # We have to explicitly choose GHC 8.10.7 because 9.0.2 is the default for
  # nixos-22.11 (which we don't want because the Stack version we use, LTS
  # 18.28, uses 8.10.7).
  hpg8107 = pkgs.haskell.packages.ghc8107;
  hpg96 = pkgs.haskell.packages.ghc96;
  melby-client = hpg8107.callPackage ../client/melby-client.nix {};
  melby-client-rust = pkgs.callPackage ../client-rust/melby-client-rust.nix {};
  # FIXME: We need to pass in a "version" attribute to all Haskell packages
  # because the default buildStackProject helper does not provide a version.
  # This means we need to start creating version strings, probably in the format
  # "YY.MM.DD-<sha>" to make it incremental.
  #melby-client-2 = (import melby-client-1) {};
  #melby-client-3 = pkgs.haskell.lib.addBuildTool melby-client-2 fakeGit;
  melby-renderer = hpg8107.callPackage ../daemon/lib/melbyd/renderer/melby-renderer.nix {};
  melby-daemon = pkgs.callPackage ../daemon/melby-daemon.nix {};
  melby-nifs = pkgs.callPackage ../daemon/lib/melbyd/nifs/melby-nifs.nix {};
in
  {
    inherit melby-client melby-client-rust melby-renderer melby-daemon melby-nifs;
  }
