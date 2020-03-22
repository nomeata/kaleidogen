Kaleidogen
==========

http://kaleidogen.nomeata.de/


Local development with GHC
--------------------------

This does not require GHCJS.

* Install GHC-8.2 and an up-to-date version of `cabal`
* Add this to your `~/.cabal/config`:

  ```
  repository ghcjs-overlay
    url: http://hackage-ghcjs-overlay.nomeata.de/
    secure: True
    root-keys:
    key-threshold: 0
  ```
  (See http://hackage-ghcjs-overlay.nomeata.de/ for more details.)
* If ghc-8.2 is not just `ghc` in the path, add
  ```
  with-compiler: /path/to/ghc-8.2
  ```
  to `cabal.project.local`.
* Run `cabal update`
* Run `cabal new-build kaleidogen` to build
* Run `cabal new-run kaleidogen` to run a webserver at http://127.0.0.1:3003/
* Run `ghcid` if you have `ghcid` installed to run the webserver and
  automatically reload it upon code changes.

Building the SDL version
------------------------

There is also an SDL version. To build and run:

* Run `cabal new-run kaleidogen-sdl`

Rough nix notes
----------------

This may speed up building


    substituters = https://cache.nixos.org https://nixcache.reflex-frp.org https://static-haskell-nix.cachix.org https://hydra.iohk.io https://tttool.cachix.org
    trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= ryantrinkle.com-1:JJiAKaRv9mWgpVAz8dwewnZe0AzzEAzPkagE9SP5NWI= static-haskell-nix.cachix.org-1:Q17HawmAwaM1/BfIxaEDKAxwTOyRVhPG5Ji9K3+FvUU= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= tttool.cachix.org-1:e/5HpIa6ZqwatH07kmO7di1p9K+AMrgkNHl/OGUUMzU= hydra.oregon.dfinity.build-2:KMTixHrh9DpAjF/0xU/49VEtNuGzQ71YaVIUSOLUaCM= cache.dfinity.systems-1:IcOn/2SVyPGOi8i3hKhQOlyiSQotiOBKwTFmyPX5YNw= zh-nix.dfinity.systems-1:9JqR5gk86r+HiJusR20x7S3e9wYFrLph7H0jwNnwTy0=


Building the SDL package using

    nix-build -A kaleidogen-sdl

but produces

    kaleidogen-sdl: SDLCallFailed {sdlExceptionCaller = "SDL.Video.glCreateContext", sdlFunction = "SDL_GL_CreateContext", sdlExceptionError = "Invalid window"}

maybe due to https://github.com/NixOS/nixpkgs/issues/9415. Needs more investigation.

