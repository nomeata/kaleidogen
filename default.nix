let sources = import (builtins.fetchurl {
    url = https://raw.githubusercontent.com/nmattia/niv/v0.2.16/nix/sources.nix;
    sha256 = "03fl8wfm2nhdiws7pmfz2kcbf47mv2f8gk30fzg4m07gb5zdv6gv";
  }) { sourcesFile = ./nix/sources.json; } ; in

let pkgs = import sources.nixpkgs {}; in
let ghcjsPkgs = pkgs; in
let staticPkgs = pkgs.pkgsStatic; in

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
          owner = "nomeata";
          repo = "haskell-telegram-api";
          # branch kaleidogen-fork
          # see https://github.com/klappvisor/haskell-telegram-api/pull/131
          rev = "0fe586a19883329ce11963189759bc044898f670";
          sha256 = "sha256-0akdSUrpqgNuWkzPOzcIUb3Szt7pOaweMNDgYPRJKGk=";
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
      telegram-api = self.callPackage telegram-api-pkg {};
      aws-lambda-haskell-runtime = pkgs.haskell.lib.appendPatch
        (unmarkBroken super.aws-lambda-haskell-runtime)
        (pkgs.fetchpatch {
          # https://github.com/theam/aws-lambda-haskell-runtime/pull/121
          url = "https://github.com/theam/aws-lambda-haskell-runtime/commit/fa19268282a5afff7aa0ba8babc723d835bed4f1.patch";
          sha256 = "sha256-pHxd3Ox5IIwJhG5bFROqotqmIu60zZ5NkBRRfz8xcnk=";
        });
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
       hex-text random-shuffle
       cryptonite memory
       temporary typed-process
       aeson aws-lambda-haskell-runtime
       JuicyPixels base64-bytestring
       telegram-api servant-client
    ];
    license = pkgs.lib.licenses.bsd3;
    configureFlags = [
      "-flambda"
    ] ++ pkgs.lib.optionals (!strip) [
      "--disable-executable-stripping"
    ] ;
    doCheck = false;
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
      zlib = dontCheck super.zlib;

      # test suite fails
      time-compat = dontCheck super.time-compat;

      # Just loops
      text-short = dontCheck super.text-short;

      # missing dependency
      jsaddle = overrideCabal super.jsaddle (drv: {
        libraryHaskellDepends = drv.libraryHaskellDepends ++ [ self.ghcjs-base ];
      });

      # bump to newer version
      ghcjs-base = super.ghcjs-base.overrideAttrs(d: {
        src = pkgs.fetchFromGitHub {
          owner = "ghcjs";
          repo = "ghcjs-base";
          rev = "fbaae59b05b020e91783df122249095e168df53f";
          sha256 = "sha256-x6eCAK1Hne0QkV3Loi9YpxbleNHU593E4AO8cbk2vUc=";
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
      hex-text random-shuffle
      ghcjs-dom jsaddle jsaddle-dom
      file-embed JuicyPixels
    ];
    license = pkgs.lib.licenses.bsd3;
    configureFlags = [
      "-f-lambda -fjsaddle -f-android -f-clib -f-sdl"
    ];
    doCheck = false;
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
      hex-text random-shuffle
      ghcjs-dom jsaddle jsaddle-dom
      file-embed JuicyPixels
      # The following only for local
      jsaddle-warp warp
      # For SDL
      OpenGL StateVar linear sdl2
    ];
    testHaskellDepends =  with haskellPackages; [
      QuickCheck tasty tasty-quickcheck
    ];
    license = pkgs.lib.licenses.bsd3;
    configureFlags = [
      "-f-lambda -fjsaddle -f-android -f-clib -fsdl"
    ] ;
  };

  index-html = pkgs.writeText "index.html" ''
    <!DOCTYPE html>
    <html>
      <head>
      </head>
      <body>
      </body>
      <script language="javascript" src="all.min.js" defer></script>
    </html>
  '';

  gh-page = pkgs.runCommandNoCC "gh-page" {
    buildInputs = [pkgs.closurecompiler];
  } ''
    mkdir -p $out
    mkdir -p $out/demo

    cp ${kaleidogen-web}/bin/kaleidogen.jsexe/runmain.js $out
    echo "Running closure-compiler"
    closure-compiler --compilation_level=ADVANCED_OPTIMIZATIONS --jscomp_off=checkVars \
      --warning_level QUIET \
      --externs=${kaleidogen-web}/bin/kaleidogen.jsexe/all.js.externs ${kaleidogen-web}/bin/kaleidogen.jsexe/all.js > $out/all.min.js
    cp ${kaleidogen-web}/bin/kaleidogen-demo.jsexe/runmain.js $out/demo
    cp ${index-html} $out/index.html

    echo "Running closure-compiler (demo)"
    closure-compiler --compilation_level=ADVANCED_OPTIMIZATIONS --jscomp_off=checkVars \
      --warning_level QUIET \
      --externs=${kaleidogen-web}/bin/kaleidogen-demo.jsexe/all.js.externs ${kaleidogen-web}/bin/kaleidogen-demo.jsexe/all.js > $out/demo/all.min.js
    cp ${index-html} $out/demo/index.html
  '';

  shell = haskellPackages.shellFor {
    packages = p: [ kaleidogen-local ];
    buildInputs = [
      pkgs.cabal-install
      pkgs.ghcid
      pkgs.closurecompiler
      haskellPackages.haskell-language-server
    ];
  };


in
  { inherit
      kaleidogen-lambda
      kaleidogen-web
      kaleidogen-local
      function-zip
      shell
      gh-page;
  }
