{ sources ? import ./nix/sources.nix }:     # import the sources
with
  { overlay = _: pkgs:
      { niv = import sources.niv {};    # use the sources :)
      };
  };
let pkgs = (import (sources.nixpkgs-static + "/survey/default.nix") {}).pkgs; in
#let nixpkgs = (import sources.nixpkgs { overlays = [ overlay ] ; config = {}; }); in
#let nixpkgs = import (sources.nixpkgs-static + "/nixpkgs.nix"); in
#let pkgs = nixpkgs.pkgsMusl; in

let compiler = "ghc865"; in
let strip = true; in

let
  kaleidogen = { mkDerivation, base, stdenv,
    MonadRandom, colour, exceptions,
    hashable, hex-text, random-shuffle,
    cryptonite, memory,
    aeson, serverless-haskell, temporary, typed-process,
  }:
      mkDerivation {
        pname = "kaleidogen";
        version = "0.1.0.0";
        src = pkgs.lib.sourceByRegex ./. [
          ".*\.cabal$"
          "LICENSE"
          "^src.*"
        ];
        isLibrary = false;
        isExecutable = true;
        enableSharedExecutables = false;
        enableSharedLibraries = false;
        executableHaskellDepends = [ base
	    MonadRandom colour exceptions
	    hashable hex-text random-shuffle
	    cryptonite memory
	    aeson serverless-haskell temporary typed-process
	];
        license = stdenv.lib.licenses.bsd3;
        configureFlags = [
	  "-flambda"
          "--ghc-option=-optl=-static"
          "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
          "--extra-lib-dirs=${pkgs.zlib.static}/lib"
          "--extra-lib-dirs=${pkgs.libffi.overrideAttrs (old: { dontDisableStatic = true; })}/lib"
        ] ++ pkgs.lib.optionals (!strip) [
          "--disable-executable-stripping"
        ] ;
      };

  normalHaskellPackages = pkgs.haskell.packages.${compiler};

  haskellPackages = with pkgs.haskell.lib; normalHaskellPackages.override {
    overrides = self: super: {
      # Dependencies we need to patch
      hpc-coveralls = appendPatch super.hpc-coveralls (builtins.fetchurl https://github.com/guillaume-nargeot/hpc-coveralls/pull/73/commits/344217f513b7adfb9037f73026f5d928be98d07f.patch);
    };
  };

  drv = haskellPackages.callPackage kaleidogen {};

in
  if pkgs.lib.inNixShell then drv.env else drv
