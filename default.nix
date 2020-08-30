let
  pkgs = import ~/build/nixpkgs {
    config = {
      packageOverrides = pkgs: rec {
        haskellPackages = pkgs.haskellPackages.override {
          overrides = haskellPackagesNew: haskellPackagesOld: rec {
            kaleidogen = haskellPackagesNew.callPackage ./project0.nix { };
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
  };

  platform = import /home/jojo/build/haskell/reflex/reflex-platform {
    config = {
      android_sdk = {
        accept_license = true;
      };
    };
    haskellOverlays = [ overlay ];
  };
  android = platform.android.buildApp {
    package = p: p.kaleidogen;
    executableName = "kaleidogen-android-clib";
    applicationId = "de.nomeata.kaleidogen";
    displayName = "Kaleidogen";
    permissions = ''
      <uses-permission android:name="android.permission.WRITE_EXTERNAL_STORAGE"/>
    '';
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

in { inherit kaleidogen-sdl kaleidogen-android android android-sdl android-run; }
