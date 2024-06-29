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
  ptu = hpg96.callPackage ../ptu.nix {};
  melby-daemon = pkgs.callPackage ../daemon/melby-daemon.nix {};
  melby-nifs = pkgs.callPackage ../daemon/lib/melbyd/nifs/melby-nifs.nix {};
in
  { melby-ptu = ptu;
    inherit melby-client melby-client-rust melby-renderer melby-daemon melby-nifs;
  }
