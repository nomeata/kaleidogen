{ mkDerivation, base, bytestring, colour, containers, formatting
, hashable, hex-text, linear, MonadRandom, mtl, OpenGL, sdl2
, random-shuffle
, StateVar, stdenv, text
, lib
}:
mkDerivation {
  pname = "kaleidogen";
  version = "0.1";
  src = builtins.filterSource (path: type:
    let relPath = lib.removePrefix (toString ./. + "/") (toString path); in
    relPath == "kaleidogen.cabal" ||
    relPath == "LICENSE" ||
    relPath == "src" ||
    (lib.hasPrefix "src" relPath && lib.hasSuffix ".hs" relPath) ||
    lib.hasPrefix "vendor" relPath
  ) ./.;
  configureFlags = [ "-f-jsaddle" "-f-android" "-fsdl" ];
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring colour containers formatting hashable hex-text
    linear MonadRandom mtl OpenGL sdl2 StateVar text random-shuffle
  ];
  homepage = "https://github.com/nomeata/kaleidogen";
  description = "Grow kaleidoscopes";
  license = stdenv.lib.licenses.mit;
}
