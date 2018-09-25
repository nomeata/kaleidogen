Kaleidogen
==========


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
* Run `cabal new-build` to build
* Run `cabal new-run` to run a webserver at http://127.0.0.1:3003/
* Run `ghcid` if you have `ghcid` installed to run the webserver and
  automatically reload it upon code changes.
