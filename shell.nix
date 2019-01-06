with builtins;
with import (fetchTarball {
  name   = "nixpkgs1709";
  url    = https://github.com/NixOS/nixpkgs/archive/17.09.tar.gz;
  sha256 = "0kpx4h9p1lhjbn1gsil111swa62hmjs9g93xmsavfiki910s73sh";
}) {};
with lib;

runCommand "arbitrary-haskell-env"
  {
    buildInputs = [ cabal-install haskell.packages.ghc7103.ghc ];
    src         = filterSource (path: _: ! (elem (baseNameOf path) [
                                 "dist" "dist-newstyle" "shell.nix"
                               ]))
                               ./.;
  }
  ''
    export HOME="$PWD/home"
    mkdir "$HOME"

    cp -r "$src" ./src
    chmod +w -R ./src
    pushd ./src
      cabal update
      cabal new-test
    popd
    mv ./src "$out"
  ''
