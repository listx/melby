let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  git = builtins.fetchGit ../.;
  gitRev =
    if builtins.pathExists ../.git
    then git.dirtyShortRev or git.shortRev
    else "unknown";
  melby-client = pkgs.callPackage ../client/melby-client.nix { inherit gitRev; };
  melby-daemon = pkgs.callPackage ../daemon/melby-daemon.nix { inherit gitRev; };
  melby-nifs = pkgs.callPackage ../daemon/lib/melbyd/nifs/melby-nifs.nix {};
in
  {
    inherit melby-client melby-daemon melby-nifs;
  }
