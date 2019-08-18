{ mkDerivation, hostPlatform, base, bytestring, colour
, containers, formatting, hashable, hex-text, linear, MonadRandom
, mtl, OpenGL, sdl2, android-activity, StateVar, stdenv, text
, jsaddle, ghcjs-dom, jsaddle-dom, reflex-dom, random-shuffle
, file-embed
, lib
, use-sdl ? false
}:
let isAndroid = hostPlatform.libc == "bionic";
in mkDerivation {
  pname = "kaleidogen";
  version = "0.1";
  src = builtins.filterSource (path: type:
    let relPath = lib.removePrefix (toString ./. + "/") (toString path); in
    relPath == "kaleidogen.cabal" ||
    relPath == "LICENSE" ||
    relPath == "src" ||
    (lib.hasPrefix "src" relPath && lib.hasSuffix ".hs" relPath)
  ) ./.;
  configureFlags = if isAndroid then
      if use-sdl
      then [ "-f-jsaddle -fandroid -fsdl -f-clib" ]
      else [ "-f-jsaddle -fandroid -f-sdl -fclib" ]
    else [ "-f-jsaddle -f-android -fsdl" ];
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bytestring colour containers
    hashable hex-text linear MonadRandom mtl StateVar text
    random-shuffle file-embed
  ] ++ (
    if isAndroid then
      if use-sdl
      then [ OpenGL sdl2 android-activity ]
      else [ ghcjs-dom jsaddle-dom jsaddle reflex-dom ]
    else [ OpenGL sdl2 ]
  );
  homepage = "https://github.com/nomeata/kaleidogen";
  description = "Grow kaleidoscopes";
  license = stdenv.lib.licenses.mit;
}
