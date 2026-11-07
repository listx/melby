{ lib, beamPackages, overrides ? (x: y: {}) }:

let
  buildRebar3 = lib.makeOverridable beamPackages.buildRebar3;
  buildMix = lib.makeOverridable beamPackages.buildMix;
  buildErlangMk = lib.makeOverridable beamPackages.buildErlangMk;

  self = packages // (overrides self packages);

  packages = with beamPackages; with self; {
    cachex = buildMix rec {
      name = "cachex";
      version = "4.1.1";

      src = fetchHex {
        pkg = "cachex";
        version = "${version}";
        sha256 = "d6b7449ff98d6bb92dda58bd4fc3189cae9f99e7042054d669596f56dc503cd8";
      };

      beamDeps = [ eternal ex_hash_ring jumper sleeplocks unsafe ];
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
      version = "2.20.0";

      src = fetchHex {
        pkg = "cowboy";
        version = "${version}";
        sha256 = "a0be47464a2887f71c061ce90723dc17441c9b81ba9569cd9b596eb0367d1edd";
      };

      beamDeps = [ cowlib ranch ];
    };

    cowlib = buildRebar3 rec {
      name = "cowlib";
      version = "2.21.0";

      src = fetchHex {
        pkg = "cowlib";
        version = "${version}";
        sha256 = "0960248773762a5319b3c17285028bc76f54f58ef0f9451367401bdf67b46274";
      };

      beamDeps = [];
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

    ex_hash_ring = buildMix rec {
      name = "ex_hash_ring";
      version = "6.0.4";

      src = fetchHex {
        pkg = "ex_hash_ring";
        version = "${version}";
        sha256 = "89adabf31f7d3dfaa36802ce598ce918e9b5b33bae8909ac1a4d052e1e567d18";
      };

      beamDeps = [];
    };

    expo = buildMix rec {
      name = "expo";
      version = "1.1.1";

      src = fetchHex {
        pkg = "expo";
        version = "${version}";
        sha256 = "5fb308b9cb359ae200b7e23d37c76978673aa1b06e2b3075d814ce12c5811640";
      };

      beamDeps = [];
    };

    file_system = buildMix rec {
      name = "file_system";
      version = "1.1.1";

      src = fetchHex {
        pkg = "file_system";
        version = "${version}";
        sha256 = "7a15ff97dfe526aeefb090a7a9d3d03aa907e100e262a0f8f7746b78f8f87a5d";
      };

      beamDeps = [];
    };

    flow = buildMix rec {
      name = "flow";
      version = "1.2.4";

      src = fetchHex {
        pkg = "flow";
        version = "${version}";
        sha256 = "874adde96368e71870f3510b91e35bc31652291858c86c0e75359cbdd35eb211";
      };

      beamDeps = [ gen_stage ];
    };

    gen_stage = buildMix rec {
      name = "gen_stage";
      version = "1.3.2";

      src = fetchHex {
        pkg = "gen_stage";
        version = "${version}";
        sha256 = "0ffae547fa777b3ed889a6b9e1e64566217413d018cabd825f786e843ffe63e7";
      };

      beamDeps = [];
    };

    gettext = buildMix rec {
      name = "gettext";
      version = "1.0.2";

      src = fetchHex {
        pkg = "gettext";
        version = "${version}";
        sha256 = "eab805501886802071ad290714515c8c4a17196ea76e5afc9d06ca85fb1bfeb3";
      };

      beamDeps = [ expo ];
    };

    googleapis = buildMix rec {
      name = "googleapis";
      version = "0.1.0";

      src = fetchHex {
        pkg = "googleapis";
        version = "${version}";
        sha256 = "1989a7244fd17d3eb5f3de311a022b656c3736b39740db46506157c4604bd212";
      };

      beamDeps = [ protobuf ];
    };

    gproc = buildRebar3 rec {
      name = "gproc";
      version = "1.3.0";

      src = fetchHex {
        pkg = "gproc";
        version = "${version}";
        sha256 = "143a8ce6eb59610a351178482b629d2b052c69df84c9c50b9c13ae38354c8c14";
      };

      beamDeps = [];
    };

    grpc = buildMix rec {
      name = "grpc";
      version = "1.0.6";

      src = fetchHex {
        pkg = "grpc";
        version = "${version}";
        sha256 = "06fec9d757a87cc2cacda6f3a8f5bb7223dc9a3ae38e6b464db0971d820c987d";
      };

      beamDeps = [ grpc_core ];
    };

    grpc_core = buildMix rec {
      name = "grpc_core";
      version = "1.0.6";

      src = fetchHex {
        pkg = "grpc_core";
        version = "${version}";
        sha256 = "06fba8299410520901a3a7971253fb86834feeb149310b1b380cd4ca31f64090";
      };

      beamDeps = [ googleapis jason protobuf telemetry ];
    };

    grpc_server = buildMix rec {
      name = "grpc_server";
      version = "1.0.6";

      src = fetchHex {
        pkg = "grpc_server";
        version = "${version}";
        sha256 = "acf9047321d8e52b41bf8ad681d9d9512a268f7c0eac18c5b9ed5b9a923f21a8";
      };

      beamDeps = [ cowboy cowlib flow grpc_core protobuf ];
    };

    jason = buildMix rec {
      name = "jason";
      version = "1.4.5";

      src = fetchHex {
        pkg = "jason";
        version = "${version}";
        sha256 = "b0c823996102bcd0239b3c2444eb00409b72f6a140c1950bc8b457d836b30684";
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
      version = "1.5.1";

      src = fetchHex {
        pkg = "luerl";
        version = "${version}";
        sha256 = "abf88d849baa0d5dca93b245a8688d4de2ee3d588159bb2faf51e15946509390";
      };

      beamDeps = [];
    };

    phoenix_pubsub = buildMix rec {
      name = "phoenix_pubsub";
      version = "2.4.1";

      src = fetchHex {
        pkg = "phoenix_pubsub";
        version = "${version}";
        sha256 = "352fba7a87bf61395b6e97da66944ad9b1bfe5459e5981039f60d68270bd9700";
      };

      beamDeps = [];
    };

    protobuf = buildMix rec {
      name = "protobuf";
      version = "0.17.1";

      src = fetchHex {
        pkg = "protobuf";
        version = "${version}";
        sha256 = "76770ecda0ef057e540932f15b5a143ba8877875e88727f53429a7dc4dbf8b6c";
      };

      beamDeps = [ jason ];
    };

    ranch = buildRebar3 rec {
      name = "ranch";
      version = "2.3.0";

      src = fetchHex {
        pkg = "ranch";
        version = "${version}";
        sha256 = "6168ec49409d982f7cfbd83dd083144f6cbe67caa4036551d2f0a3ad67c9d023";
      };

      beamDeps = [];
    };

    rustler = buildMix rec {
      name = "rustler";
      version = "0.38.0";

      src = fetchHex {
        pkg = "rustler";
        version = "${version}";
        sha256 = "704c03c1bf66be12b031c5a389347b91c81c5cb819a24b068b0de36fe4a5652a";
      };

      beamDeps = [ jason ];
    };

    sleeplocks = buildRebar3 rec {
      name = "sleeplocks";
      version = "1.1.4";

      src = fetchHex {
        pkg = "sleeplocks";
        version = "${version}";
        sha256 = "bc12752ab0693ea4e4a3bcf4e063cef408d71197a3c0fad75497fabd475f5481";
      };

      beamDeps = [];
    };

    telemetry = buildRebar3 rec {
      name = "telemetry";
      version = "1.4.2";

      src = fetchHex {
        pkg = "telemetry";
        version = "${version}";
        sha256 = "928f6495066506077862c0d1646609eed891a4326bee3126ba54b60af61febb1";
      };

      beamDeps = [];
    };

    temp = buildMix rec {
      name = "temp";
      version = "0.4.9";

      src = fetchHex {
        pkg = "temp";
        version = "${version}";
        sha256 = "bc8bf7b27d9105bef933ef4bf4ba37ac6b899dbeba329deaa88c60b62d6b4b6d";
      };

      beamDeps = [];
    };

    timex = buildMix rec {
      name = "timex";
      version = "4.0.0";

      src = fetchHex {
        pkg = "timex";
        version = "${version}";
        sha256 = "74118ceb13fd7b75abc6804511fc0484438a6ca51e8e03fc94520cfb12868035";
      };

      beamDeps = [ combine gettext tzdata ];
    };

    tz = buildMix rec {
      name = "tz";
      version = "0.28.4";

      src = fetchHex {
        pkg = "tz";
        version = "${version}";
        sha256 = "72fe8d526cf30fcc4e07cec4ceb583ba5f2d69c6334c9533419c836a6acd1c56";
      };

      beamDeps = [];
    };

    tzdata = buildMix rec {
      name = "tzdata";
      version = "1.2.2";

      src = fetchHex {
        pkg = "tzdata";
        version = "${version}";
        sha256 = "4a2435eb3614711b220a9f9f9b56204d14518c6a84e94278cc8b697ca62221a7";
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
      version = "2.6.0";

      src = fetchHex {
        pkg = "wait_for_it";
        version = "${version}";
        sha256 = "18c8bddd2c5fb45e8246244f1503a2a250d62eb04e73be53effcc0639e18ce05";
      };

      beamDeps = [ telemetry ];
    };
  };
in self

