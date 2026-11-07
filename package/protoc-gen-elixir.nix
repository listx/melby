{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs {} }:
let
  packages = pkgs.beam.packagesWith pkgs.beam.interpreters.erlang_28;
  mixFodDeps = packages.fetchMixDeps {
    pname = "mix-deps-${pname}";
    inherit src version;
    sha256 = "sha256-T1uL3xXXmCkobJJhS3p6xMrJUyiim3AMwaG87/Ix7A8=";
  };
  pname = "protoc-gen-elixir";
  src = pkgs.fetchgit {
    url = "https://github.com/elixir-protobuf/protobuf";
    rev = "v${version}";
    sha256 = "sha256-XSTB4MATvLf1sj5O1QEc62Jm3PUR9Wt4tKY03xW4yvI=";
  };
  version = "0.17.1";
in
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
