{ mkDerivation, base, haskell-generate, QuickCheck, regex-posix
, stdenv, tasty, tasty-quickcheck
}:
mkDerivation {
  pname = "ArbitraryHaskell";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ base haskell-generate QuickCheck ];
  testDepends = [
    base haskell-generate QuickCheck regex-posix tasty tasty-quickcheck
  ];
  homepage = "http://chriswarbo.net/git/arbitrary-haskell";
  description = "Generate Arbitrary Strings of Haskell code";
  license = stdenv.lib.licenses.publicDomain;
}
