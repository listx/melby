{ lib, beamPackages, overrides ? (x: y: {}) }:

let
  buildRebar3 = lib.makeOverridable beamPackages.buildRebar3;
  buildMix = lib.makeOverridable beamPackages.buildMix;
  buildErlangMk = lib.makeOverridable beamPackages.buildErlangMk;

  self = packages // (overrides self packages);

  packages = with beamPackages; with self; {
    cachex = buildMix rec {
      name = "cachex";
      version = "3.6.0";

      src = fetchHex {
        pkg = "cachex";
        version = "${version}";
        sha256 = "ebf24e373883bc8e0c8d894a63bbe102ae13d918f790121f5cfe6e485cc8e2e2";
      };

      beamDeps = [ eternal jumper sleeplocks unsafe ];
    };

    certifi = buildRebar3 rec {
      name = "certifi";
      version = "2.12.0";

      src = fetchHex {
        pkg = "certifi";
        version = "${version}";
        sha256 = "ee68d85df22e554040cdb4be100f33873ac6051387baf6a8f6ce82272340ff1c";
      };

      beamDeps = [];
    };

    combine = buildMix rec {
      name = "combine";
      version = "0.10.0";

      src = fetchHex {
        pkg = "combine";
        version = "${version}";
        sha256 = "1b1dbc1790073076580d0d1d64e42eae2366583e7aecd455d1215b0d16f2451b";
      };

      beamDeps = [];
    };

    cowboy = buildErlangMk rec {
      name = "cowboy";
      version = "2.12.0";

      src = fetchHex {
        pkg = "cowboy";
        version = "${version}";
        sha256 = "8a7abe6d183372ceb21caa2709bec928ab2b72e18a3911aa1771639bef82651e";
      };

      beamDeps = [ cowlib ranch ];
    };

    cowlib = buildRebar3 rec {
      name = "cowlib";
      version = "2.13.0";

      src = fetchHex {
        pkg = "cowlib";
        version = "${version}";
        sha256 = "e1e1284dc3fc030a64b1ad0d8382ae7e99da46c3246b815318a4b848873800a4";
      };

      beamDeps = [];
    };

    elixir_make = buildMix rec {
      name = "elixir_make";
      version = "0.8.4";

      src = fetchHex {
        pkg = "elixir_make";
        version = "${version}";
        sha256 = "6e7f1d619b5f61dfabd0a20aa268e575572b542ac31723293a4c1a567d5ef040";
      };

      beamDeps = [ certifi ];
    };

    eternal = buildMix rec {
      name = "eternal";
      version = "1.2.2";

      src = fetchHex {
        pkg = "eternal";
        version = "${version}";
        sha256 = "2c9fe32b9c3726703ba5e1d43a1d255a4f3f2d8f8f9bc19f094c7cb1a7a9e782";
      };

      beamDeps = [];
    };

    expo = buildMix rec {
      name = "expo";
      version = "0.5.2";

      src = fetchHex {
        pkg = "expo";
        version = "${version}";
        sha256 = "8c9bfa06ca017c9cb4020fabe980bc7fdb1aaec059fd004c2ab3bff03b1c599c";
      };

      beamDeps = [];
    };

    file_system = buildMix rec {
      name = "file_system";
      version = "1.0.0";

      src = fetchHex {
        pkg = "file_system";
        version = "${version}";
        sha256 = "6752092d66aec5a10e662aefeed8ddb9531d79db0bc145bb8c40325ca1d8536d";
      };

      beamDeps = [];
    };

    gettext = buildMix rec {
      name = "gettext";
      version = "0.24.0";

      src = fetchHex {
        pkg = "gettext";
        version = "${version}";
        sha256 = "bdf75cdfcbe9e4622dd18e034b227d77dd17f0f133853a1c73b97b3d6c770e8b";
      };

      beamDeps = [ expo ];
    };

    gproc = buildRebar3 rec {
      name = "gproc";
      version = "1.0.0";

      src = fetchHex {
        pkg = "gproc";
        version = "${version}";
        sha256 = "109f253c2787de8a371a51179d4973230cbec6239ee673fa12216a5ce7e4f902";
      };

      beamDeps = [];
    };

    grpc = buildMix rec {
      name = "grpc";
      version = "0.8.1";

      src = fetchHex {
        pkg = "grpc";
        version = "${version}";
        sha256 = "1cccd9fd83547a562f315cc0e1ee1879546f0a44193b5c8eb8d68dae0bb2065b";
      };

      beamDeps = [ cowboy cowlib gun jason mint protobuf telemetry ];
    };

    gun = buildRebar3 rec {
      name = "gun";
      version = "2.1.0";

      src = fetchHex {
        pkg = "gun";
        version = "${version}";
        sha256 = "52fc7fc246bfc3b00e01aea1c2854c70a366348574ab50c57dfe796d24a0101d";
      };

      beamDeps = [ cowlib ];
    };

    hackney = buildRebar3 rec {
      name = "hackney";
      version = "1.20.1";

      src = fetchHex {
        pkg = "hackney";
        version = "${version}";
        sha256 = "fe9094e5f1a2a2c0a7d10918fee36bfec0ec2a979994cff8cfe8058cd9af38e3";
      };

      beamDeps = [ certifi idna metrics mimerl parse_trans ssl_verify_fun unicode_util_compat ];
    };

    hpax = buildMix rec {
      name = "hpax";
      version = "1.0.0";

      src = fetchHex {
        pkg = "hpax";
        version = "${version}";
        sha256 = "7f1314731d711e2ca5fdc7fd361296593fc2542570b3105595bb0bc6d0fad601";
      };

      beamDeps = [];
    };

    idna = buildRebar3 rec {
      name = "idna";
      version = "6.1.1";

      src = fetchHex {
        pkg = "idna";
        version = "${version}";
        sha256 = "92376eb7894412ed19ac475e4a86f7b413c1b9fbb5bd16dccd57934157944cea";
      };

      beamDeps = [ unicode_util_compat ];
    };

    jason = buildMix rec {
      name = "jason";
      version = "1.4.3";

      src = fetchHex {
        pkg = "jason";
        version = "${version}";
        sha256 = "9a90e868927f7c777689baa16d86f4d0e086d968db5c05d917ccff6d443e58a3";
      };

      beamDeps = [];
    };

    jumper = buildMix rec {
      name = "jumper";
      version = "1.0.2";

      src = fetchHex {
        pkg = "jumper";
        version = "${version}";
        sha256 = "9b7782409021e01ab3c08270e26f36eb62976a38c1aa64b2eaf6348422f165e1";
      };

      beamDeps = [];
    };

    luerl = buildRebar3 rec {
      name = "luerl";
      version = "1.2.0";

      src = fetchHex {
        pkg = "luerl";
        version = "${version}";
        sha256 = "9cafd4f6094ff0f5a9d278fd81d60d3e026c820bdfb6cacd4b1bd909f21b525d";
      };

      beamDeps = [];
    };

    metrics = buildRebar3 rec {
      name = "metrics";
      version = "1.0.1";

      src = fetchHex {
        pkg = "metrics";
        version = "${version}";
        sha256 = "69b09adddc4f74a40716ae54d140f93beb0fb8978d8636eaded0c31b6f099f16";
      };

      beamDeps = [];
    };

    mimerl = buildRebar3 rec {
      name = "mimerl";
      version = "1.3.0";

      src = fetchHex {
        pkg = "mimerl";
        version = "${version}";
        sha256 = "a1e15a50d1887217de95f0b9b0793e32853f7c258a5cd227650889b38839fe9d";
      };

      beamDeps = [];
    };

    mint = buildMix rec {
      name = "mint";
      version = "1.6.2";

      src = fetchHex {
        pkg = "mint";
        version = "${version}";
        sha256 = "5ee441dffc1892f1ae59127f74afe8fd82fda6587794278d924e4d90ea3d63f9";
      };

      beamDeps = [ hpax ];
    };

    muontrap = buildMix rec {
      name = "muontrap";
      version = "1.4.1";

      src = fetchHex {
        pkg = "muontrap";
        version = "${version}";
        sha256 = "c12a3469974f99b25a0cfa4bcf45aa15d122a1384308f602ca1c20c16f195729";
      };

      beamDeps = [ elixir_make ];
    };

    parse_trans = buildRebar3 rec {
      name = "parse_trans";
      version = "3.4.1";

      src = fetchHex {
        pkg = "parse_trans";
        version = "${version}";
        sha256 = "620a406ce75dada827b82e453c19cf06776be266f5a67cff34e1ef2cbb60e49a";
      };

      beamDeps = [];
    };

    phoenix_pubsub = buildMix rec {
      name = "phoenix_pubsub";
      version = "2.1.3";

      src = fetchHex {
        pkg = "phoenix_pubsub";
        version = "${version}";
        sha256 = "bba06bc1dcfd8cb086759f0edc94a8ba2bc8896d5331a1e2c2902bf8e36ee502";
      };

      beamDeps = [];
    };

    protobuf = buildMix rec {
      name = "protobuf";
      version = "0.12.0";

      src = fetchHex {
        pkg = "protobuf";
        version = "${version}";
        sha256 = "75fa6cbf262062073dd51be44dd0ab940500e18386a6c4e87d5819a58964dc45";
      };

      beamDeps = [ jason ];
    };

    ranch = buildRebar3 rec {
      name = "ranch";
      version = "1.8.0";

      src = fetchHex {
        pkg = "ranch";
        version = "${version}";
        sha256 = "49fbcfd3682fab1f5d109351b61257676da1a2fdbe295904176d5e521a2ddfe5";
      };

      beamDeps = [];
    };

    rustler = buildMix rec {
      name = "rustler";
      version = "0.33.0";

      src = fetchHex {
        pkg = "rustler";
        version = "${version}";
        sha256 = "7c4752728fee59a815ffd20c3429c55b644041f25129b29cdeb5c470b80ec5fd";
      };

      beamDeps = [ jason toml ];
    };

    sleeplocks = buildRebar3 rec {
      name = "sleeplocks";
      version = "1.1.3";

      src = fetchHex {
        pkg = "sleeplocks";
        version = "${version}";
        sha256 = "d3b3958552e6eb16f463921e70ae7c767519ef8f5be46d7696cc1ed649421321";
      };

      beamDeps = [];
    };

    ssl_verify_fun = buildRebar3 rec {
      name = "ssl_verify_fun";
      version = "1.1.7";

      src = fetchHex {
        pkg = "ssl_verify_fun";
        version = "${version}";
        sha256 = "fe4c190e8f37401d30167c8c405eda19469f34577987c76dde613e838bbc67f8";
      };

      beamDeps = [];
    };

    telemetry = buildRebar3 rec {
      name = "telemetry";
      version = "1.2.1";

      src = fetchHex {
        pkg = "telemetry";
        version = "${version}";
        sha256 = "dad9ce9d8effc621708f99eac538ef1cbe05d6a874dd741de2e689c47feafed5";
      };

      beamDeps = [];
    };

    temp = buildMix rec {
      name = "temp";
      version = "0.4.7";

      src = fetchHex {
        pkg = "temp";
        version = "${version}";
        sha256 = "6af19e7d6a85a427478be1021574d1ae2a1e1b90882586f06bde76c63cd03e0d";
      };

      beamDeps = [];
    };

    timex = buildMix rec {
      name = "timex";
      version = "3.7.11";

      src = fetchHex {
        pkg = "timex";
        version = "${version}";
        sha256 = "8b9024f7efbabaf9bd7aa04f65cf8dcd7c9818ca5737677c7b76acbc6a94d1aa";
      };

      beamDeps = [ combine gettext tzdata ];
    };

    toml = buildMix rec {
      name = "toml";
      version = "0.7.0";

      src = fetchHex {
        pkg = "toml";
        version = "${version}";
        sha256 = "0690246a2478c1defd100b0c9b89b4ea280a22be9a7b313a8a058a2408a2fa70";
      };

      beamDeps = [];
    };

    tz = buildMix rec {
      name = "tz";
      version = "0.26.5";

      src = fetchHex {
        pkg = "tz";
        version = "${version}";
        sha256 = "c4f9392d710582c7108b6b8c635f4981120ec4b2072adbd242290fc842338183";
      };

      beamDeps = [ mint ];
    };

    tzdata = buildMix rec {
      name = "tzdata";
      version = "1.1.1";

      src = fetchHex {
        pkg = "tzdata";
        version = "${version}";
        sha256 = "a69cec8352eafcd2e198dea28a34113b60fdc6cb57eb5ad65c10292a6ba89787";
      };

      beamDeps = [ hackney ];
    };

    unicode_util_compat = buildRebar3 rec {
      name = "unicode_util_compat";
      version = "0.7.0";

      src = fetchHex {
        pkg = "unicode_util_compat";
        version = "${version}";
        sha256 = "25eee6d67df61960cf6a794239566599b09e17e668d3700247bc498638152521";
      };

      beamDeps = [];
    };

    unsafe = buildMix rec {
      name = "unsafe";
      version = "1.0.2";

      src = fetchHex {
        pkg = "unsafe";
        version = "${version}";
        sha256 = "b485231683c3ab01a9cd44cb4a79f152c6f3bb87358439c6f68791b85c2df675";
      };

      beamDeps = [];
    };

    wait_for_it = buildMix rec {
      name = "wait_for_it";
      version = "2.1.2";

      src = fetchHex {
        pkg = "wait_for_it";
        version = "${version}";
        sha256 = "c47be5ad9db9c24ef28d24dc27977243b8bc7ca366bf3e36d56af0093ef257d1";
      };

      beamDeps = [];
    };
  };
in self

