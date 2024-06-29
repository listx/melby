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

{ sources ? import ../../../../package/nix/sources.nix }:
let
  files = [
    ./Cargo.toml
    ./Cargo.lock
    ./.gitignore
    ./src
    ./src/path_shorten.rs
    ./src/color.rs
    ./src/lib.rs
    ./Makefile
  ];

  src = builtins.path {
    name = "melby-nifs";
    path = ./.;
    filter = path: type: builtins.elem (/. + path) files;
  };
  # Overlays
  oxalica-rust-overlay = import sources.rust-overlay;
  # Build rust crates.
  naersk = pkgs.callPackage sources.naersk {};
  # The final "pkgs" attribute with all the bells and whistles of our overlays.
  pkgs = import sources.nixpkgs {
    overlays = [
      oxalica-rust-overlay
    ];
  };
in
  naersk.buildPackage {
    inherit src;
    copyBins = false;
    copyTarget = true;
    # See https://pyo3.rs/v0.14.2/building_and_distribution.html#macos (by way
    # of https://github.com/PyO3/pyo3/issues/1800#issuecomment-1071890916).
    CARGO_BUILD_RUSTFLAGS = pkgs.lib.optional
      pkgs.stdenv.isDarwin
      "-C link-arg=-undefined -C link-arg=dynamic_lookup";
  }
