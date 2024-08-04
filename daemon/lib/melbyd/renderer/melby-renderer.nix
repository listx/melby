let
  sources = import ../../../../package/nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  fakeGit = pkgs.writeShellScriptBin "git"
  ''
    echo -unknown
  '';
  files = [
    ./LICENSE
    ./cabal.project
    ./lib
    ./lib/MelbyRenderer
    ./lib/MelbyRenderer/Colorizer.hs
    ./lib/MelbyRenderer/GitVersion.hs
    ./lib/MelbyRenderer/Log.hs
    ./lib/MelbyRenderer/melby_renderer.proto
    ./lib/MelbyRenderer/PathAliases.hs
    ./lib/MelbyRenderer/Schema.hs
    ./lib/MelbyRenderer/Widgets.hs
    ./melby-renderer.cabal
    ./melby-renderer.hs
    ./melby-renderer.nix
    ./stack.yaml
    ./stack.yaml.lock
  ];
  macOS-security =
    # Make `/usr/bin/security` available in `PATH`, which is needed for stack
    # on darwin which calls this binary to find certificates. See
    # https://github.com/tweag/rules_haskell/commit/31171a520f49f263895112678ac93c7ed958ead1.
    pkgs.writeScriptBin "security" ''exec /usr/bin/security "$@"'';
in

{ ghc }:

pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "melby-renderer";
  src = ./.;
  buildInputs = [
    pkgs.zlib
    fakeGit
  ] ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [pkgs.llvm_12 macOS-security];
}
