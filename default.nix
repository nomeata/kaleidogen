let sources = import (builtins.fetchurl {
    url = https://raw.githubusercontent.com/nmattia/niv/v0.2.16/nix/sources.nix;
    sha256 = "03fl8wfm2nhdiws7pmfz2kcbf47mv2f8gk30fzg4m07gb5zdv6gv";
  }) { sourcesFile = ./nix/sources.json; } ; in

let pkgs = import sources.nixpkgs {}; in
let ghcjsPkgs = pkgs; in
#let staticPkgs = (import (sources.nixpkgs-static + "/survey/default.nix") {}).pkgs; in
let staticPkgs = pkgs.pkgsMusl; in

let strip = true; in

let
  telegram-api-pkg =
    { mkDerivation
    , stdenv
    , fetchFromGitHub
    , aeson
    , containers
    , http-api-data
    , http-client
    , servant
    , servant-client
    , servant-client-core
    , mtl
    , text
    , transformers
    , http-media
    , http-types
    , mime-types
    , string-conversions
    , binary
    , ansi-wl-pprint
    , hjpath
    , hspec
    , http-client-tls
    , optparse-applicative
    }:
      mkDerivation {
        pname = "telegram-api";
        version = "0.7.1.0";
        src = fetchFromGitHub {
          owner = "klappvisor";
          repo = "haskell-telegram-api";
          rev = "abbfd76c40f2783c113b660184a03cc94d58e751";
          sha256 = "0mzhigdyj5jdwycmz587s05zp5c7wcf7njw3x866iln59kp0rgi3";
        };
        isLibrary = true;
        isExecutable = false;
        enableSharedExecutables = false;
        enableSharedLibraries = false;
        libraryHaskellDepends = [
	    aeson
            containers
            http-api-data
            http-client
            servant
            servant-client
            servant-client-core
            mtl
            text
            transformers
            http-media
            http-types
            mime-types
            string-conversions
            binary
            ansi-wl-pprint
            hjpath
            hspec
            http-client-tls
            optparse-applicative
            servant-client
	];
        jailbreak = true;  # want servant-client-0.16.0.1, not 0.16
        license = pkgs.lib.licenses.bsd3;
      };

  kaleidogen-src = pkgs.lib.sourceByRegex ./. [
    ".*\.cabal$"
    "LICENSE"
    "^src.*"
    "^vendor.*"
  ];

  staticHaskellPackages = with pkgs.haskell.lib; staticPkgs.haskellPackages.override {
    overrides = self: super: {
      # Dependencies we need to patch
      hpc-coveralls = appendPatch super.hpc-coveralls (builtins.fetchurl https://github.com/guillaume-nargeot/hpc-coveralls/pull/73/commits/344217f513b7adfb9037f73026f5d928be98d07f.patch);
      telegram-api = self.callPackage telegram-api-pkg {};
      aws-lambda-haskell-runtime = unmarkBroken super.aws-lambda-haskell-runtime;
    };
  };
  kaleidogen-lambda = staticHaskellPackages.mkDerivation {
    pname = "kaleidogen-lambda";
    version = "0.1.0.0";
    src = kaleidogen-src;
    isLibrary = false;
    isExecutable = true;
    enableSharedExecutables = false;
    enableSharedLibraries = false;
    executableHaskellDepends = with staticHaskellPackages; [
       base
       MonadRandom colour exceptions
       hashable hex-text random-shuffle
       cryptonite memory
       temporary typed-process
       aeson aws-lambda-haskell-runtime
       JuicyPixels base64-bytestring
       telegram-api servant-client
    ];
    license = pkgs.lib.licenses.bsd3;
    configureFlags = [
     "-flambda"
      "--ghc-option=-optl=-static"
      "--extra-lib-dirs=${staticPkgs.gmp6.override { withStatic = true; }}/lib"
      "--extra-lib-dirs=${staticPkgs.zlib.static}/lib"
      "--extra-lib-dirs=${staticPkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
    ] ++ pkgs.lib.optionals (!strip) [
      "--disable-executable-stripping"
    ] ;

  };

  function-zip = pkgs.runCommandNoCC "kaleidogen-function.zip" {
    buildInputs = [ pkgs.zip ];
  } ''
    mkdir -p $out
    cp ${kaleidogen-lambda}/bin/kaleidogen-amazon-lambda bootstrap
    zip $out/function.zip bootstrap
  '';

  ghcjsHaskellPackages = with pkgs.haskell.lib; ghcjsPkgs.haskell.packages.ghcjs.override {
    overrides = self: super: {
      ghcjs-dom-jsffi = unmarkBroken super.ghcjs-dom-jsffi;

      # System.Process.createPipeInternal: not yet supported on GHCJS
      QuickCheck = dontCheck super.QuickCheck;

      # gets stuck running the tests in node, it seems
      scientific = dontCheck super.scientific;
      tasty-quickcheck = dontCheck super.tasty-quickcheck;

      # test suite fails
      time-compat = dontCheck super.time-compat;

      # missing dependency
      jsaddle = overrideCabal super.jsaddle (drv: {
        libraryHaskellDepends = drv.libraryHaskellDepends ++ [ self.ghcjs-base ];
      });

      # uses git: URL, can be fixed when upgrading to nixpkgs da2cbf54e8ec3e66e2e24d2c55823bce98a7f96b
      ghcjs-base = super.ghcjs-base.overrideAttrs(d: {
        src = pkgs.fetchFromGitHub {
          owner = "ghcjs";
          repo = "ghcjs-base";
          rev = "01014ade3f8f5ae677df192d7c2a208bd795b96c";
          sha256 = "0qr05m0djll3x38dhl85pl798arsndmwfhil8yklhb70lxrbvfrs";
        };
      });
    };
  };
  kaleidogen-web = ghcjsHaskellPackages.mkDerivation {
    pname = "kaleidogen";
    version = "0.1.0.0";
    src = kaleidogen-src;
    isLibrary = false;
    isExecutable = true;
    executableHaskellDepends = with ghcjsHaskellPackages; [
      base
      MonadRandom colour exceptions
      hashable hex-text random-shuffle
      ghcjs-dom jsaddle jsaddle-dom
      file-embed
    ];
    license = pkgs.lib.licenses.bsd3;
    configureFlags = [
      "-f-lambda -fjsaddle -f-android -f-clib -f-sdl"
    ];
  };

  haskellPackages = with pkgs.haskell.lib; pkgs.haskellPackages.override {
    overrides = self: super: {
      ghcjs-dom-jsffi = unmarkBroken super.ghcjs-dom-jsffi;
      jsaddle-warp = unmarkBroken super.jsaddle-warp;
    };
  };
  kaleidogen-local = haskellPackages.mkDerivation {
    pname = "kaleidogen";
    version = "0.1.0.0";
    src = kaleidogen-src;
    isLibrary = false;
    isExecutable = true;
    executableHaskellDepends =  with haskellPackages; [
      base
      MonadRandom colour exceptions
      hashable hex-text random-shuffle
      ghcjs-dom jsaddle jsaddle-dom
      file-embed
      # The following only for local
      jsaddle-warp warp
    ];
    license = pkgs.lib.licenses.bsd3;
    preConfigure = ''
      find /build/setup-package.conf.d
    '';
    configureFlags = [
      "-f-lambda -fjsaddle -f-android -f-clib -f-sdl"
    ] ;
  };

  gh-page = pkgs.runCommandNoCC "gh-page" {} ''
    mkdir -p $out
    cp -rv ${kaleidogen-web}/bin/kaleidogen.jsexe/* $out
    cp -rv ${kaleidogen-web}/bin/kaleidogen-demo.jsexe $out/demo
  '';

  shell = kaleidogen-lambda.env.overrideAttrs(old: {
    preferLocalBuild = true;
    allowSubstitutes = true;
  });

in
  { inherit
      kaleidogen-lambda
      kaleidogen-web
      kaleidogen-local
      function-zip
      shell
      gh-page;
  }
