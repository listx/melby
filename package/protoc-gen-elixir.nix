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
