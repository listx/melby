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

{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {} }:
let
  packages = pkgs.beam.packagesWith pkgs.beam.interpreters.erlangR26;
  mixFodDeps = packages.fetchMixDeps {
    pname = "mix-deps-${pname}";
    inherit src version;
    sha256 = "sha256-9ZHYtxgdvYO8rjVU10OtkJuWNMjpU/4dCX01DO7pu14=";
  };
  pname = "protoc-gen-elixir";
  src = pkgs.fetchgit {
    url = "https://github.com/elixir-protobuf/protobuf";
    rev = "v${version}";
    sha256 = "sha256-J9HEISTDda3E2D20Okw3rhpe29W3qATr0rrfUoQ+81I=";
  };
  version = "0.11.0";
in
  # FIXME: This is very heavy as it compiles the entire project (which we don't
  # need to do). However this appears to be the only way possible to compile the
  # protoc-gen-elixir escript executable.
  packages.mixRelease {
    inherit pname src version mixFodDeps;
    buildPhase = ''
      MIX_ENV=prod mix do escript.build --force
    '';

    installPhase = ''
      mkdir -p $out/bin
      mv protoc-gen-elixir $out/bin
    '';
  }
