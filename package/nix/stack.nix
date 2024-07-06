let
  sources = import ./sources.nix;
  pkgs = import sources.nixpkgs { };
in

# See https://docs.haskellstack.org/en/stable/nix_integration/#using-a-custom-shellnix-file
#
# But basically this tells stack to figure out the GHC version by looking at
# stack.yaml (we have to have this file in $PWD from the current nix-shell
# session).
#
# This is really great because this means we can technically have different
# tools use different versions of GHC. So maybe one of the smaller, simpler
# tools can use the latest GHC with useful features and have that as a role
# model for the other Haskell binaries that are lagging behind on older GHC
# versions due to dependencies that have not yet upgraded to the newer version.
{ ghc }:

pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "melby";
  # System dependencies needed at compilation time
  buildInputs = [
    pkgs.zlib
  ];
}
