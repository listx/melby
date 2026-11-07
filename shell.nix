let
  # Nixpkgs snapshot.
  sources = import ./package/nix/sources.nix;
  # Overlays
  oxalica-rust-overlay = import sources.rust-overlay;
  # Build rust crates.
  naersk = final: prev: {
    naersk = pkgs.callPackage sources.naersk
      {
        inherit sources;
      };
    };
  # The final "pkgs" attribute with all the bells and whistles of our overlays.
  pkgs = import sources.nixpkgs {
    overlays = [
      oxalica-rust-overlay
      naersk
    ];
  };

  protoc-gen-elixir = pkgs.callPackage ./package/protoc-gen-elixir.nix {};
  darwin-cargo-build-rustflags = pkgs.lib.attrsets.optionalAttrs pkgs.stdenv.isDarwin {
    CARGO_BUILD_RUSTFLAGS = "-C link-arg=-undefined -C link-arg=dynamic_lookup";
  };
  # See https://nixos.wiki/wiki/TexLive for customizing texlive packages
  # (picking only those packages we need as in here).
  tex = (pkgs.texlive.combine {
    inherit (pkgs.texlive) scheme-basic
      dvisvgm
      fontspec
      luatex85
      pgf
      sourcesanspro
      standalone
      xkeyval;
  });

  # Lilac (the Haskell version) for tangling and weaving. This comes with its
  # own pinned nixpkgs, so it does not depend on our nixpkgs snapshot above.
  lilacSrc = builtins.fetchGit {
    url = "https://github.com/listx/lilac";
    ref = "refs/tags/v0.3.0.0";
    name = "lilac";
    submodules = true;
  };
  lilacPkg = import "${lilacSrc}/nix/release.nix" { rev = lilacSrc.rev; };
in

# This is our development shell.
pkgs.mkShell ({
  buildInputs = [
    # Rust
    pkgs.rust-bin.stable.latest.default
    pkgs.rust-analyzer

    # Elixir
    pkgs.beam.packages.erlangR26.elixir
    pkgs.beam.packages.erlangR26.erlang # erl, escript
    pkgs.mix2nix

    # gRPC and Protocol Buffers
    pkgs.protobuf
    # Needed for running Elixir plugin for protoc. E.g., "protoc
    # --elixir_out=plugins=grpc:. foo.proto". We need to install
    # protoc-gen-elixir.
    protoc-gen-elixir

    # Tangling and weaving for Literate Programming.
    lilacPkg
    tex

    # For updating Nix dependencies.
    pkgs.niv

    # Misc
    pkgs.git
    pkgs.less
  ]
  # For file_system on Linux.
  ++ pkgs.lib.optional pkgs.stdenv.isLinux pkgs.inotify-tools
  # For file_system on macOS. See
  # https://elixirforum.com/t/could-not-compile-file-system-watcher-for-mac/17432/10
  # for a discussion about getting a file system watcher working on Mac.
  ++ pkgs.lib.optionals pkgs.stdenv.isDarwin (with pkgs.darwin.apple_sdk.frameworks; [
    CoreFoundation
    CoreServices
    Security
  ])
  ++ pkgs.lib.optionals pkgs.stdenv.isDarwin [
    # This allows mix2nix to be able to download hex packages using HTTPS, in a
    # `nix-shell --pure` environment. See
    # https://github.com/NixOS/nixpkgs/issues/66716#issuecomment-883399373.
    pkgs.cacert
  ];

  shellHook = ''
    # Prefer to use melbyc from the local development tree path.
    export PATH=$PWD/daemon/test:$PWD/client/target/debug:$PATH
    export KUBECONFIG=$PWD/daemon/test/fake_kube_config
    export MELBY_DIR=$PWD/daemon/test/sample
    export MELBY_PROJECT_ROOT=$PWD
    export LUA_PATH="$PWD/daemon/test/sample/?.lua"
  '';

  # Make Elixir not complain about using "latin1" encoding.
  LANG = "en_US.UTF-8";
  LOCALE_ARCHIVE = pkgs.lib.optional pkgs.stdenv.isLinux
    "${pkgs.glibcLocales}/lib/locale/locale-archive";
} // darwin-cargo-build-rustflags)
