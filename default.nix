{ mkDerivation, base, haskell-generate, QuickCheck, stdenv, tasty
, tasty-quickcheck
}:
mkDerivation {
  pname = "ArbitraryHaskell";
  version = "0.1.0.0";
  src = ./.;
  buildDepends = [ base ];
  testDepends = [
    base haskell-generate QuickCheck tasty tasty-quickcheck
  ];
  homepage = "http://chriswarbo.net/git/arbitrary-haskell";
  description = "Generate Arbitrary Strings of Haskell code";
  license = stdenv.lib.licenses.publicDomain;
}
