let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};
  melby = import ./build.nix;
in
  pkgs.symlinkJoin {
    name = "melby";
    paths = with melby; [
      melby-client
      melby-client-rust
      melby-renderer
      melby-daemon
    ];
    # If users have already installed individual packages already, make the ones
    # we bundle here act as a fallback. Default priority is 5, and so by setting
    # it to 10, makes Nix treat the files of this derivation as a fallback if
    # there is a conflict.
    meta.priority = 10;
  }
