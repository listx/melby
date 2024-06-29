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
  sources = import package/nix/sources.nix;
  pkgs = import sources.nixpkgs { };
  fakeGit = pkgs.writeShellScriptBin "git"
  ''
    echo -unknown
  '';
  files = [
    ./LICENSE
    ./cabal.project
    ./lib
    ./lib/PostTangleUtil
    ./lib/PostTangleUtil/GitVersion.hs
    ./ptu.cabal
    ./ptu.hs
    ./ptu.nix
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
  name = "melby-ptu";
  # We use this builtin because we want to use our own name for the folder.
  # Otherwise, the surrounding folder name "doc" gets used, which is bad.
  src = builtins.path {
    name = "melby-ptu";
    path = ./.;
    filter = path: type: builtins.elem (/. + path) files;
  };
  buildInputs = [
    pkgs.zlib
    fakeGit
  ] ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [pkgs.llvm_10 macOS-security];
}
