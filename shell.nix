{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, directory, haskell-generate, process
      , QuickCheck, regex-posix, stdenv, tasty, tasty-quickcheck
      }:
      mkDerivation {
        pname = "ArbitraryHaskell";
        version = "0.1.0.0";
        src = ./.;
        buildDepends = [ base directory haskell-generate QuickCheck ];
        testDepends = [
          base directory haskell-generate process QuickCheck regex-posix
          tasty tasty-quickcheck
        ];
        doCheck = false;
        homepage = "http://chriswarbo.net/git/arbitrary-haskell";
        description = "Generate Arbitrary Strings of Haskell code";
        license = stdenv.lib.licenses.publicDomain;
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
