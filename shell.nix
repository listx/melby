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

  # Wrap Stack to configure Nix integration and target the correct Stack-Nix
  # file
  #
  # - nix: Enable Nix support (use Stack's built-in Nix integration; this makes
  #   Stack populate the "ghc" variable in our nix/stack.nix file with the GHC
  #   version found in the stack.yaml file; this way we don't have to specify a
  #   GHC version in Nix (when we already define it transitively through
  #   specifying Stack's LTS version in stack.yaml)). This also makes it so that
  #   ghci is not available in the nix-shell; rather it can only be accessed
  #   through "stack exec -- ghci".
  #
  # - nix-pure: Do not pass environment variables, like `NIX_PATH`
  #
  # - nix-shell-file: Specify the Nix file to use (otherwise it uses `shell.nix`
  #   by default)
  #
  # Vanilla (unwrapped) stack is available as `.stack-wrapped`, courtesy of
  # pkgs.symlinkJoin.
  stack-wrapped = pkgs.symlinkJoin {
    name = "stack";
    paths = [ pkgs.stack ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/stack \
        --add-flags "\
          --nix \
          --nix-pure \
          --system-ghc \
          --no-install-ghc \
          --nix-shell-file=./stack.nix \
        "
    '';
  };

  protoc-gen-elixir = pkgs.callPackage ./package/protoc-gen-elixir.nix {};
  darwin-cargo-build-rustflags = pkgs.lib.attrsets.optionalAttrs pkgs.stdenv.isDarwin {
    CARGO_BUILD_RUSTFLAGS = "-C link-arg=-undefined -C link-arg=dynamic_lookup";
  };
  macOS-security =
    # Make `/usr/bin/security` available in `PATH`, which is needed for stack
    # on darwin which calls this binary to find certificates. See
    # https://github.com/tweag/rules_haskell/commit/31171a520f49f263895112678ac93c7ed958ead1.
    pkgs.writeScriptBin "security" ''exec /usr/bin/security "$@"'';

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
in

# This is our development shell.
pkgs.mkShell ({
  buildInputs = [
    # Haskell
    stack-wrapped
    # Let stack use Nix with the Nix we version here.
    pkgs.nix

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
    pkgs.emacs29-nox
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
    macOS-security
  ];

  # FIXME: how to get the git version into the environment...???? The setting
  # below for MELBY_PROJECT_ROOT only works for the dev env, not the release env
  shellHook = ''
    # Prefer to use melbyr and melbyc from the local development tree path.
    export PATH=$PWD/daemon/test:$PWD/daemon/lib/melbyd/renderer:$PWD/client:$PATH
    export KUBECONFIG=$PWD/daemon/test/fake_kube_config
    export MELBY_DIR=$PWD/daemon/test/sample
    export MELBY_PROJECT_ROOT=$PWD
    export MELBYR_PATH=$PWD/daemon/lib/melbyd/renderer/melbyr
    export LUA_PATH="$PWD/daemon/test/sample/?.lua"
  '';

  # Configure the Nix path to our own `pkgs`, to ensure Stack-with-Nix uses the
  # correct one rather than the global <nixpkgs> when looking for the right
  # `ghc` argument to pass in `nix/stack.nix`
  #
  # See https://nixos.org/nixos/nix-pills/nix-search-paths.html for more
  # information.
  NIX_PATH = "nixpkgs=" + pkgs.path;

  # Make Elixir not complain about using "latin1" encoding.
  LANG = "en_US.UTF-8";
  LOCALE_ARCHIVE = pkgs.lib.optional pkgs.stdenv.isLinux
    "${pkgs.glibcLocales}/lib/locale/locale-archive";
} // darwin-cargo-build-rustflags)
