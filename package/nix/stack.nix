# Copyright 2024 Linus Arver
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

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
