let
  sources = import ../package/nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  # Nix never copies the .git directory as input to generate a derivation,
  # because Git's .git directory is not deterministic. So we never have a Git
  # directory, which means any "use git to derive the version" command will
  # fail. One way around this is to fetch a previous version of this repo (using
  # fetchGit) and then use the revision there as an output here using string
  # interpolation. We then make this fake Git script take precedence over the
  # vanilla git binary, tricking our version generation code. See
  # https://jeancharles.quillet.org/posts/2022-04-22-Embed-the-git-hash-into-a-binary-with-nix.html
  # and
  # https://discourse.nixos.org/t/accessing-git-directory-in-flake-with-local-source/17370/7.
  fakeGit = pkgs.writeShellScriptBin "git"
  ''
    echo -unknown
  '';
  files = [
    ./LICENSE
    ./cabal.project
    ./lib
    ./lib/MelbyClient
    ./lib/MelbyClient/GitVersion.hs
    ./lib/MelbyClient/melby_client.proto
    ./lib/MelbyClient/Schema.hs
    ./melby-client.cabal
    ./melby-client.hs
    ./melby-client.nix
    ./stack.yaml
    ./stack.yaml.lock
  ];
  macOS-security =
    # Make `/usr/bin/security` available in `PATH`, which is needed for stack
    # on darwin which calls this binary to find certificates. See
    # https://github.com/tweag/rules_haskell/commit/31171a520f49f263895112678ac93c7ed958ead1.
    pkgs.writeScriptBin "security" ''exec /usr/bin/security "$@"'';
in

# When we tell  See https://docs.haskellstack.org/en/stable/nix_integration/#using-a-custom-shellnix-file.
{ ghc }:

pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "melby-client";
  src = builtins.path {
    name = "melby-client";
    path = ./.;
    filter = path: type: builtins.elem (/. + path) files;
  };
  # System dependencies needed at compilation time.
  buildInputs = [
    pkgs.zlib
    fakeGit
  ]
  # See
  # https://www.reddit.com/r/haskell/comments/rjm0x8/help_wanted_for_llvm_config_for_haskell_on_mac/
  # about needing a version of LLVM < 13 on Mac.
  ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [pkgs.llvm_10 macOS-security];
}
