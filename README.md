Kaleidogen
==========

Procedural art; check it out on <http://kaleidogen.nomeata.de/>

This also runs as a Telegram bot at <https://t.me/InternetComputerBot>.

Local development with GHC
--------------------------

If you want to hack on this, its easiest to use the nix setup (although you can
also use plain `cabal` probably):

1. [Install `nix`](https://nixos.org/download.html)
2. Add the Kaleidogen nix cache at <https://kaleidogen.cachix.org/>
3. Enter the nix shell with `nix-shell`
4. Run `cabal run kaleidogen` to build and run Kaleidogen locally
5. Open <http://127.0.0.1:3003/> in Chrome
   ([Firefox doesn’t work well.](https://github.com/ghcjs/jsaddle/issues/64))

For an even faster feedback loop, you can run `ghcid` to run the webserver and
have it automatically restart when you change the code. You still have to
`ctrl-r` the browser window. (See the `.ghcid` file).

Kaleidogen can build for various targets (Web, Android, SDL, Amazon lambda /
Telegram), and so the `kaleidogen.cabal` file has many flags to enable or
disable various components. The default is for building with `jsaddle`, i.e.
for web or local development.

Building artefacts
------------------

The above builds a native Linux binary that runs Kaleidogen, and controls the
browser using WebSocket, using `jsaddle`, which is good for local development.
You can build this binary with

    nix-build -A kaleidogen-local

it also builds the `kaleidogen-sdl` program.


The “real” version, though, runs purely in the browser, and is built using
`ghcjs`. You can build that using

    nix-build -A kaleidogen-web

and to get the actual source for <https://kaleidogen.nomeata.de>, use

    nix-build -A gh-page

If the kaleidogen nix cache does not have the right packages, or you did not
set it up correctly, this will build a _lot_ of things (such as ghcjs).


The Telegram Bot runs on Amazon Lambda, and needs to be compiled statically.
This, too, requires a warm cache or it will build a lot of things (such as a
complete static environment):

    # the binary
    nix-build -A kaleidogen-lambda
    # the lambda function zip file
    nix-build -A function-zip
    # Deploy locally (master branch deploys automatically)
    aws lambda update-function-code --region us-east-2 --function-name kaleidogen --zip-file fileb://result/function.zip

There is an experimental Andorid build using reflex-platform, but it has not
made it to the `master` branch yet, see branch `android`.

Building the SDL version
------------------------

There is also an SDL version. To build and run:

* Run `cabal new-run kaleidogen-sdl`

