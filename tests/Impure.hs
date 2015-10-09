module Main where

import Control.Exception (try, SomeException)
import Data.Either
import Data.List
import System.Directory
import System.Exit
import System.IO.Temp
import System.Process
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Tasty (defaultMain, testGroup, localOption)
import Test.Tasty.QuickCheck
import Test.Arbitrary.Haskell
import Test.Arbitrary.Internal.Cabal
import Text.Regex.Posix

withOptions = localOption (QuickCheckTests 10)

main = do cabal <- haveCabal
          let tests = if cabal then allTests else hsTests
          defaultMain $ withOptions tests

allTests = testGroup "All impure tests" [hsTests, cabalTests]

hsTests = testGroup "Impure Haskell tests" [
            testProperty "Only Main modules are generated" moduleIsMain
          ]

cabalTests = testGroup "Impure Cabal tests" [
    testProperty "Cabal dir exists"  cabalDirExists
  , testProperty "Cabal file exists" cabalFileExists
  , testProperty "Cabal project is valid" cabalProjectValid
  , testProperty "Cabal project can be configured" cabalProjectConfigures
  , testProperty "Files are written" cabalFilesAreWritten
  ]

-- Haskell tests

moduleIsMain (H s) = s =~ "^module Main\\s" :: Bool

-- Cabal tests

cabalDirExists p = monadicIO $ do
  exists <- run $ withSystemTempDirectory "arbitraryhaskelltest" checkExists
  assert exists
  where checkExists tmpDir = makeProject tmpDir p >>= doesDirectoryExist

cabalFileExists p = monadicIO $ do
  exists <- run $ withSystemTempDirectory "arbitraryhaskelltest" checkExists
  assert exists
  where checkExists tmpDir = do
          dir <- makeProject tmpDir p
          doesFileExist (dir ++ "/" ++ name p ++ ".cabal")

cabalProjectValid p = monadicIO $ do
  result <- run $ withSystemTempDirectory "arbitraryhaskelltest" checkProject
  assert (result == ExitSuccess)
  where checkProject tmpDir = do
          dir <- makeProject tmpDir p
          cabal dir ["check"]

cabalProjectConfigures p = monadicIO $ do
  result <- run $ withSystemTempDirectory "arbitraryhaskelltest" configureProject
  assert (result == ExitSuccess)
  where configureProject tmpDir = do
          dir <- makeProject tmpDir p
          cabal dir ["configure"]

cabalFilesAreWritten p = monadicIO $ do
  found <- run $ withSystemTempDirectory "arbitraryhaskelltest" findFiles
  assert (files p `allIn` found)
  where findFiles tmpDir = makeProject tmpDir p >>= filesIn
        allIn []                ys = True
        allIn (((ds, f), _):xs) ys = intercalate "/" ([""] ++ ds ++ [f]) `elem` ys &&
                                     xs `allIn` ys
        filesIn dir = do out <- readProcess "find" [dir] ""
                         return . map (drop (length dir)) . lines $ out

-- Helpers

haveCabal :: IO Bool
haveCabal = fmap isRight
              (try (rawSystem "cabal" ["--help"]) :: IO (Either SomeException ExitCode))

sanitise :: FilePath -> FilePath
sanitise = filter (`elem` alpha)

cabal :: FilePath -> [String] -> IO ExitCode
cabal d args = do
  (sin, sout, serr, p) <- createProcess (proc "cabal" args) {
                            cwd = Just d
                          }
  waitForProcess p
