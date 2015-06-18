module Main where

import Control.Exception (try, SomeException)
import Data.List
import System.Directory
import System.Exit
import System.Process
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck
import Test.Arbitrary.Haskell
import Test.Arbitrary.Cabal
import Text.Regex.Posix

-- Haskell tests

moduleIsMain (H s) = s =~ "^module Main\\s" :: Bool

hsTests = testGroup "Haskell tests" [
            testProperty "Only Main modules are generated" moduleIsMain
          ]

-- Cabal tests

cabalDirExists p = monadicIO $ cleanup $ do
  dir <- run (makeProject tmpDir p)
  exists <- run (doesDirectoryExist dir)
  assert exists

cabalFileExists p = monadicIO $ cleanup $ do
  dir <- run (makeProject tmpDir p)
  exists <- run (doesFileExist (dir ++ "/" ++ name p ++ ".cabal"))
  assert exists

cabalProjectValid p = monadicIO $ cleanup $ do
  dir <- run (makeProject tmpDir p)
  result <- run (cabal dir ["check"])
  assert (result == ExitSuccess)

cabalTests = testGroup "Cabal tests" [
    testProperty "Cabal dir exists"  cabalDirExists
  , testProperty "Cabal file exists" cabalFileExists
  , testProperty "Cabal project is valid" cabalProjectValid
  ]

-- Helpers

tmpDir = "/tmp/ArbitraryHaskellTest"

rmTmpDir :: IO (Either SomeException ())
rmTmpDir = try (removeDirectoryRecursive tmpDir)

cleanup :: PropertyM IO a -> PropertyM IO a
cleanup x = do run $ rmTmpDir
               result <- x
               run $ rmTmpDir
               return result

sanitise :: FilePath -> FilePath
sanitise = filter (`elem` alpha)

cabal :: FilePath -> [String] -> IO ExitCode
cabal d args = do (sin, sout, serr, p) <- createProcess (proc "cabal" args) {
                                            cwd = Just d
                                          }
                  waitForProcess p

cleanupInvariant = monadicIO $ do
  cleanup (return ())
  exists <- run (doesDirectoryExist tmpDir)
  assert (not exists)

cleanupCleansUp x = monadicIO $ do
  run $ createDirectoryIfMissing True (tmpDir ++ "/" ++ sanitise x)
  existsPre <- run (doesDirectoryExist tmpDir)
  assert existsPre
  cleanup (return ())
  existsPost <- run (doesDirectoryExist tmpDir)
  assert (not existsPost)

helperTests = testGroup "Helper tests" [
    testProperty (tmpDir ++ " wiped by cleanup") cleanupInvariant
  , testProperty "Cleanup cleans up" cleanupCleansUp
  ]

main = defaultMain $ testGroup "All tests" [
         hsTests
       , cabalTests
       , helperTests
       ]
