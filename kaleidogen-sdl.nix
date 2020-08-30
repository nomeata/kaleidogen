{ mkDerivation, base, bytestring, colour, containers, formatting
, hashable, hex-text, linear, MonadRandom, mtl, OpenGL, sdl2
, StateVar, stdenv, text
}:
mkDerivation {
  pname = "kaleidogen";
  version = "0.1";
  src = ./.;
  configureFlags = [ "-f-jsaddle" "-f-android" ];
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring colour containers formatting hashable hex-text
    linear MonadRandom mtl OpenGL sdl2 StateVar text
  ];
  homepage = "https://github.com/nomeata/kaleidogen";
  description = "Grow kaleidoscopes";
  license = stdenv.lib.licenses.mit;
}
