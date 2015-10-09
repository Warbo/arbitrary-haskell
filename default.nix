{ mkDerivation, base, directory, haskell-generate, process
, QuickCheck, regex-posix, stdenv, tasty, tasty-quickcheck
, temporary
}:
mkDerivation {
  pname = "ArbitraryHaskell";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base directory haskell-generate QuickCheck
  ];
  testHaskellDepends = [
    base directory haskell-generate process QuickCheck regex-posix
    tasty tasty-quickcheck temporary
  ];
  homepage = "http://chriswarbo.net/git/arbitrary-haskell";
  description = "Generate Arbitrary Strings of Haskell code";
  license = stdenv.lib.licenses.publicDomain;
}
