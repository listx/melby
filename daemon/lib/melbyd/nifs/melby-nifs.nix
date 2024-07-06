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
