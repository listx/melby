{ sources ? import ../package/nix/sources.nix }:
let
  # Overlays
  oxalica-rust-overlay = import sources.rust-overlay;
  # The final "pkgs" attribute with all the bells and whistles of our overlays.
  pkgs = import sources.nixpkgs {
    overlays = [
      oxalica-rust-overlay
    ];
  };
  systemdAvailable = pkgs.lib.meta.availableOn
    pkgs.stdenv.hostPlatform
    pkgs.systemd;
  packages = pkgs.beam.packagesWith pkgs.beam.interpreters.erlangR26;
  # FIXME: This appears to result in a SHA mismatch between Linux and Mac. We
  # should probably use mix2nix to download mix dependencies reproducibly, as
  # described in
  # https://github.com/NixOS/nixpkgs/blame/2ea8b001f6ba09cc6eb9a30f4e117cbc3ec5afe2/doc/languages-frameworks/beam.section.md#L79.
  #
  # Hmm but this appears to be the same now since upgrading to 23.11.
  mixFodDeps = packages.fetchMixDeps {
    pname = "mix-deps-${pname}";
    inherit src version;
    sha256 = "sha256-sNHiSWJgB2bRIt6i3o10EV6HL+SbqsyFoN5RBtHTQ3M=";
  };
  mixNixDeps = import ./mix.nix {
    lib = pkgs.lib;
    beamPackages = pkgs.beamPackages;
  };
  pname = "melby-daemon";
  # FIXME When this file is eventually upstreamed to Nixpkgs (if we want that),
  # at that point we can start referring to Git tags/releases and use the
  # version string from that instead (and download the tarball/zip, and proceed
  # to build the package).
  version = "0-dev";
  src = builtins.path {
    name = "melby-daemon";
    path = ./.;
    filter = path: type: builtins.elem (/. + path) files;
  };
  files = [
    ./config
    ./config/config.exs
    ./config/dev.exs
    ./config/prod.exs
    ./config/runtime.exs
    ./config/test.exs
    ./.formatter.exs
    ./.gitignore
    ./install
    ./install/melby.service
    ./install/launchctl.sh
    ./lib
    ./lib/generated
    ./lib/generated/melby_client.pb.ex
    ./lib/generated/melby_daemon.pb.ex
    ./lib/generated/melby.pb.ex
    ./lib/generated/melby_renderer.pb.ex
    ./lib/melbyd
    ./lib/melbyd/router.ex

    ./lib/melbyd/nifs
    ./lib/melbyd/nifs/Cargo.toml
    ./lib/melbyd/nifs/Cargo.lock
    ./lib/melbyd/nifs/.gitignore
    ./lib/melbyd/nifs/src
    ./lib/melbyd/nifs/src/path_shorten.rs
    ./lib/melbyd/nifs/src/color.rs
    ./lib/melbyd/nifs/src/lib.rs
    ./lib/melbyd/nifs/Makefile

    ./lib/melbyd/color.ex
    ./lib/melbyd/application.ex
    ./lib/melbyd/cache.ex
    ./lib/melbyd/standard_resource.ex
    ./lib/melbyd/view.ex
    ./lib/melbyd/grpc.ex
    ./lib/melbyd/shell_logger.ex
    ./lib/melbyd/lua_api.ex
    ./lib/melbyd/nifs.ex
    ./lib/melbyd/path.ex
    ./lib/melbyd/renderer
    ./lib/melbyd/renderer/cabal.project
    ./lib/melbyd/renderer/.ghci
    ./lib/melbyd/renderer/lib
    ./lib/melbyd/renderer/lib/MelbyRenderer
    ./lib/melbyd/renderer/lib/MelbyRenderer/melby_renderer.proto
    ./lib/melbyd/renderer/lib/MelbyRenderer/Schema.hs
    ./lib/melbyd/renderer/lib/MelbyRenderer/GitVersion.hs
    ./lib/melbyd/renderer/lib/MelbyRenderer/Log.hs
    ./lib/melbyd/renderer/melby-renderer.cabal
    ./lib/melbyd/renderer/stack.yaml.lock
    ./lib/melbyd/renderer/melby-renderer.hs
    ./lib/melbyd/renderer/.gitignore
    ./lib/melbyd/renderer/Makefile
    ./lib/melbyd/renderer/melby-renderer.nix
    ./lib/melbyd/renderer/stack.yaml
    ./lib/melbyd/renderer/LICENSE
    ./Makefile
    ./mix.exs
    ./mix.lock
    ./test
    ./test/test_helper.exs
    ./test/sample
    ./test/sample/get_pods.gotemplate
    ./test/sample/GitFake.lua
    ./test/sample/Git.lua
    ./test/sample/git_staged_bytes.sh
    ./test/sample/KubeCurrentFake.lua
    ./test/sample/KubeCurrent.lua
    ./test/sample/KubeFake.lua
    ./test/sample/Kube.lua
    ./test/sample/melby.lua
    ./test/melbyd_test.exs
  ];
  melby = import ../package/build.nix;
  nifs_ext = if pkgs.stdenv.isDarwin then "dylib" else "so";
  # For whatever reason Rustler insists on searching for .so files, even on
  # Darwin. Luckily we can just copy over the generated .dylib files to .so
  # files.
  nifs_cp_lib_cmd = pkgs.lib.strings.optionalString pkgs.stdenv.isDarwin ''
    cp _build/prod/lib/melbyd/priv/native/libmelbyd_nifs.dylib \
       _build/prod/lib/melbyd/priv/native/libmelbyd_nifs.so
  '';
  # We have to manually compile the "mac_listener" binary because the
  # "file_system" Elixir package does not do this automatically. See
  # https://gist.github.com/jbott/2030c133509e7c1db4f41941b5367475.
  mac_compile_mac_listener_cmd = pkgs.lib.strings.optionalString pkgs.stdenv.isDarwin ''
    clang \
      -framework CoreFoundation \
      -framework CoreServices \
      -Wno-deprecated-declarations deps/file_system/c_src/mac/*.c \
      -o $out/bin/mac_listener
  '';
in
  packages.mixRelease {
    inherit pname src version mixNixDeps;
    #inherit pname src version mixFodDeps;
    buildInputs = [
      pkgs.rust-bin.stable.latest.default
      pkgs.rust-analyzer
      melby.melby-nifs
      melby.melby-renderer
    ]
    # FIXME: This part is a copy/paste of shell.nix at the root and should be
    # deduplicated.
    # For file_system on Linux.
    ++ pkgs.lib.optional pkgs.stdenv.isLinux pkgs.inotify-tools
    # For file_system on macOS. See
    # https://elixirforum.com/t/could-not-compile-file-system-watcher-for-mac/17432/10
    # for a discussion about getting a file system watcher working on Mac.
    ++ pkgs.lib.optionals pkgs.stdenv.isDarwin
        (with pkgs.darwin.apple_sdk.frameworks; [
          CoreFoundation
          CoreServices
          Security
        ]);
    nativeBuildInputs = [ pkgs.makeWrapper ];
    LANG = "en_US.UTF-8";
    LOCALE_ARCHIVE = pkgs.lib.optional pkgs.stdenv.isLinux
      "${pkgs.glibcLocales}/lib/locale/locale-archive";

    # Install our Rust NIF shared object that was already compiled in a separate
    # Nix package we made (called "melby-nifs"). This is required for Rustler to
    # generate the correct beam bytecode that calls into melbyd_nifs.
    postConfigure = ''
      echo "extracting melby-nifs"
      ${pkgs.zstd}/bin/zstd -d ${melby.melby-nifs}/target.tar.zst --stdout \
        | tar -x -C ./lib/melbyd/nifs
      mkdir -p _build/prod/lib/melbyd/priv/native
      cp lib/melbyd/nifs/target/release/deps/libmelbyd_nifs.${nifs_ext} \
         _build/prod/lib/melbyd/priv/native
      ${nifs_cp_lib_cmd}
    '';

    # Make melbyr available as a runtime dependency.
    postBuild = ''
      mkdir -p $out/bin
      cp ${melby.melby-renderer}/bin/melbyr $out/bin
      ${mac_compile_mac_listener_cmd}
    '';

    # Install
    postInstall = ''
      mkdir -p $out/share/melby
      cp -r $src/test/sample $out/share/melby

      if ${ pkgs.lib.boolToString systemdAvailable }; then
        install -Dm644 $src/install/melby.service \
          $out/share/systemd/user/melby.service

        # Fix ExecStart path.
        substituteInPlace "$out/share/systemd/user/melby.service" \
          --replace 'ExecStart=melbyd' "ExecStart=$out/bin/melbyd start"
      fi

      # Make the autogenerated "melbyd" wrapper script (courtesy of mixRelease)
      # source the PATH locations of dependencies, because otherwise if we use
      # the script in a bare-bones environment (such as systemd), it will fail
      # to find binaries like "readlink" or "cut" (coreutils).
      wrapProgram $out/bin/melbyd --prefix PATH : ${pkgs.lib.makeBinPath
        (with pkgs;
            [
                coreutils
                gawk
                gnugrep
                gnused
            ])}
    '';

    # See https://github.com/whitfin/cachex/issues/205 and
    # https://github.com/NixOS/nixpkgs/pull/192472 and
    # https://github.com/erlang/otp/pull/2114. Cachex breaks with "invalid_hook"
    # if you strip binaries and beam files.
    stripDebug = false;
  }
