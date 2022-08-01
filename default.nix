let
  sources = import (builtins.fetchurl {
    url = https://raw.githubusercontent.com/nmattia/niv/v0.2.16/nix/sources.nix;
    sha256 = "03fl8wfm2nhdiws7pmfz2kcbf47mv2f8gk30fzg4m07gb5zdv6gv";
  }) { sourcesFile = ./nix/sources.json; };

  # android experiments

  android =
    let
      pkgs = import sources.nixpkgs {
        config = {
          packageOverrides = pkgs: rec {
            haskellPackages = pkgs.haskellPackages.override {
              overrides = self: super: rec {
                kaleidogen = self.callPackage ./project0.nix { };
                # haskell-activity = import /home/jojo/build/haskell/reflex/android-activity {};
              };
            };
          };
          android_sdk = {
            accept_license = true;
          };
          allowUnfree = true;
        };
      };

      kaleidogen-android = pkgs.pkgsCross.aarch64-android-prebuilt.haskellPackages.kaleidogen;

      kaleidogen-sdl = pkgs.haskellPackages.callPackage ./kaleidogen-sdl.nix { };


      overlay = self: super: {
        kaleidogen = self.callPackage ./project0.nix { };
        kaleidogen-sdl = self.callPackage ./project0.nix { use-sdl = true; };
        #kaleidogen = self.callCabal2nixWithOptions "kaleidogen" ./. "-f-sdl -fandroid" { };
        sdl2 = pkgs.haskell.lib.dontCheck super.sdl2;
        # splitmix = pkgs.haskell.lib.dontCheck super.splitmix;
        # splitmix = pkgs.haskell.lib.dontCheck (self.callPackage ./splitmix.nix {});
        # vector = pkgs.haskell.lib.dontCheck super.vector;
      };

      platform = import sources.reflex-platform {
        config = {
          android_sdk = {
            accept_license = true;
          };
        };
        haskellOverlays = [ overlay ];
      };
      buildIcons = pkgs.callPackage ./buildIcons.nix { };
      android = platform.android.buildApp {
        package = p: p.kaleidogen;
        executableName = "kaleidogen-android-clib";
        applicationId = "de.nomeata.kaleidogen";
        displayName = "Kaleidogen";
        resources = buildIcons {
          src = ./android-icon.png;
        };
        version = {
          code = "4";
          # Must be a monotonically increasing number; defines what it means to "upgrade" the app
          name = "1.0";
          # The version that is displayed to the end user
        };
        isRelease = true;
        #permissions = ''
        #  <uses-permission android:name="android.permission.WRITE_EXTERNAL_STORAGE"/>
        #'';
      };


      kaleidogen = platform.nixpkgs.haskellPackages.callPackage ./project0.nix { };
      android-run = platform.nixpkgs.androidenv.emulateApp {
        name = "emulate-MyAndroidApp";
        platformVersion = "24";
        abiVersion = "armeabi-v7a"; # mips, x86 or x86_64
        # useGoogleAPIs = false;
        app = android;
        package = "systems.obsidian";
        activity = ".HaskellActivity";
      };

      android-sdl = platform.android.buildApp {
        package = p: p.kaleidogen-sdl;
        executableName = "kaleidogen-android-sdl";
        applicationId = "de.nomeata.kaleidogen";
        displayName = "Kaleidogen";
      };

    in { inherit kaleidogen-sdl kaleidogen-android android android-sdl android-run; };

  pkgs = import sources.nixpkgs {};
  ghcjsPkgs = pkgs;
  #staticPkgs = (import (sources.nixpkgs-static + "/survey/default.nix") {}).pkgs;
  staticPkgs = pkgs.pkgsStatic;

  compiler = "ghc865";
  strip = true;

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
      file-embed
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
      file-embed
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

  gh-page = pkgs.runCommandNoCC "gh-page" {} ''
    mkdir -p $out
    cp -rv ${kaleidogen-web}/bin/kaleidogen.jsexe/* $out
    cp -rv ${kaleidogen-web}/bin/kaleidogen-demo.jsexe $out/demo
  '';

  shell = haskellPackages.shellFor {
    packages = p: [ kaleidogen-local ];
    buildInputs = [
      pkgs.cabal-install
      pkgs.ghcid
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
      gh-page
      android;
  }
